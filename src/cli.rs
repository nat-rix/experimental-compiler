use std::{
    ffi::{OsStr, OsString},
    path::PathBuf,
};

use crate::error::CliError;

macro_rules! build_compiler_flags {
    ($($f:ident: $n:literal = $e:literal),* $(,)?) => {
        #[derive(Debug, Clone)]
        pub struct CompilerFlags { $(pub $f: bool),* }

        impl Default for CompilerFlags {
            fn default() -> Self {
                Self { $( $f: $e ),* }
            }
        }

        impl CompilerFlags {
            pub fn iter_mut(&mut self) -> impl Iterator<Item = (&'static str, &mut bool)> {
                [ $(( $n, &mut self.$f )),* ].into_iter()
            }
        }
    };
}

build_compiler_flags! {
    optimize_jmps: "optimize-jmps" = true,
    debug_ir: "debug-ir" = false,
}

#[derive(Debug, Clone, Default)]
pub struct Args {
    pub skip_compiler: bool,
    pub features: CompilerFlags,
    pub input_files: Vec<PathBuf>,
    pub output_file: Option<PathBuf>,

    exec_name: Option<OsString>,
    disallow_options_parsing: bool,
    stop_parsing: bool,
}

impl Args {
    pub fn from_os_args(mut args: impl Iterator<Item = OsString>) -> Result<Self, CliError> {
        let mut result = Self {
            exec_name: args.next(),
            ..Default::default()
        };
        result.parse_os_args(args)?;
        Ok(result)
    }

    pub fn parse_os_args(
        &mut self,
        mut args: impl Iterator<Item = OsString>,
    ) -> Result<(), CliError> {
        while let Some(arg) = args.next().filter(|_| !self.stop_parsing) {
            if let Some(arg) = (!self.disallow_options_parsing)
                .then(|| arg.to_str())
                .flatten()
                .and_then(|arg| arg.strip_prefix('-'))
            {
                if let Some(arg) = arg.strip_prefix('-') {
                    self.parse_double_dash(arg, &mut args)?
                } else {
                    self.parse_single_dash(arg, &mut args)?
                };
                continue;
            }
            self.input_files.push(arg.into());
        }
        Ok(())
    }

    fn get_exec_name(&self) -> &OsStr {
        self.exec_name
            .as_deref()
            .unwrap_or_else(|| env!("CARGO_PKG_NAME").as_ref())
    }

    pub fn emit_help(&self) {
        println!(
            "usage: {} [options] --output=<output-file> [--] <input-files>...",
            std::path::Path::new(self.get_exec_name()).display()
        );
        println!();
        println!("options:");
        println!("  -f, --feature=[no-]<feature>  Set or disable a feature flag");
        println!("  -h, --help                    Print help");
        println!("  -o, --output=<file>           Place the output binary into <file>");
        println!();
        println!("features:");
        for (name, default) in CompilerFlags::default().iter_mut() {
            println!(
                "  {name} (default: {})",
                if *default { "active" } else { "inactive" }
            );
        }
    }

    fn parse_single_dash_val(
        &mut self,
        char: char,
        chars: &mut core::str::Chars,
        args: &mut impl Iterator<Item = OsString>,
    ) -> Result<OsString, CliError> {
        let str = chars.as_str();
        if str.is_empty() {
            args.next().ok_or(CliError::EmptyShortArgument(char))
        } else {
            *chars = "".chars();
            Ok(str.into())
        }
    }

    fn parse_double_dash_val(
        &mut self,
        name: &str,
        rhs: Option<&str>,
        args: &mut impl Iterator<Item = OsString>,
    ) -> Result<OsString, CliError> {
        if let Some(rhs) = rhs {
            Ok(rhs.into())
        } else {
            args.next()
                .ok_or_else(|| CliError::EmptyLongArgument(name.to_string()))
        }
    }

    fn parse_single_dash(
        &mut self,
        arg: &str,
        args: &mut impl Iterator<Item = OsString>,
    ) -> Result<(), CliError> {
        let mut chars = arg.chars();
        let mut is_empty = true;
        while let Some(char) = chars.next() {
            is_empty = false;
            match char {
                'f' => {
                    let val = self.parse_single_dash_val(char, &mut chars, args)?;
                    self.set_feature(val)?;
                }
                'h' | '?' => {
                    self.set_help();
                }
                'o' => {
                    let val = self.parse_single_dash_val(char, &mut chars, args)?;
                    self.set_output(val);
                }
                _ => return Err(CliError::UnknownShortArgument(char)),
            }
        }
        if is_empty {
            self.input_files.push(arg.into());
        }
        Ok(())
    }

    fn parse_double_dash(
        &mut self,
        arg: &str,
        args: &mut impl Iterator<Item = OsString>,
    ) -> Result<(), CliError> {
        let (arg, rhs) = arg
            .split_once('=')
            .map(|(lhs, rhs)| (lhs, Some(rhs)))
            .unwrap_or((arg, None));
        match arg {
            "" => {
                self.disallow_options_parsing = true;
            }
            "feature" => {
                let val = self.parse_double_dash_val(arg, rhs, args)?;
                self.set_feature(val)?;
            }
            "help" | "?" => {
                self.set_help();
            }
            "output" => {
                let val = self.parse_double_dash_val(arg, rhs, args)?;
                self.set_output(val);
            }
            _ => return Err(CliError::UnknownLongArgument(arg.to_string())),
        }
        Ok(())
    }

    pub fn set_feature(&mut self, raw_name: OsString) -> Result<(), CliError> {
        let raw_name = raw_name.to_string_lossy();
        let mut name = raw_name.as_ref();
        let mut is_active = true;
        if let Some(new_name) = name.strip_prefix("no-") {
            is_active = false;
            name = new_name;
        }
        if let Some((_, flag)) = self.features.iter_mut().find(|(n, ..)| *n == name) {
            *flag = is_active;
            Ok(())
        } else {
            Err(CliError::UnknownFeature(name.to_string()))
        }
    }

    pub fn set_help(&mut self) {
        self.emit_help();
        self.skip_compiler = true;
        self.stop_parsing = true;
    }

    pub fn set_output(&mut self, out: OsString) {
        self.output_file = Some(out.into());
    }

    pub fn get_input(&self) -> Result<&PathBuf, CliError> {
        match self.input_files.as_slice() {
            [] => Err(CliError::MissingInputFile),
            [file] => Ok(file),
            [_, _, ..] => Err(CliError::TooManyInputFiles),
        }
    }

    pub fn get_output(&self) -> Result<&PathBuf, CliError> {
        self.output_file.as_ref().ok_or(CliError::MissingOutputFile)
    }
}
