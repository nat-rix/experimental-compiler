use std::{ffi::OsString, path::PathBuf};

use crate::error::CliError;

#[derive(Debug, Clone, Default)]
pub struct Args {
    pub input_files: Vec<PathBuf>,
    pub output_file: Option<PathBuf>,

    disallow_options_parsing: bool,
}

impl Args {
    pub fn from_os_args(mut args: impl Iterator<Item = OsString>) -> Result<Self, CliError> {
        let _ = args.next();
        let mut result = Self::default();
        result.parse_os_args(args)?;
        Ok(result)
    }

    pub fn parse_os_args(
        &mut self,
        mut args: impl Iterator<Item = OsString>,
    ) -> Result<(), CliError> {
        while let Some(arg) = args.next() {
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
            "output" => {
                let val = self.parse_double_dash_val(arg, rhs, args)?;
                self.set_output(val);
            }
            _ => return Err(CliError::UnknownLongArgument(arg.to_string())),
        }
        Ok(())
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
