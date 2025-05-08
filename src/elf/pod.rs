/// The type is valid to get its underlying memory representation as a continuous slice.
///
/// # Safety
///
/// See `bytemuck::NoUninit`.
pub unsafe trait AsBytes: Sized + Copy + 'static {
    fn as_bytes(&self) -> &[u8] {
        let slice: &[Self] = core::slice::from_ref(self);
        let size = core::mem::size_of_val(slice);
        unsafe { core::slice::from_raw_parts(slice.as_ptr() as _, size) }
    }
}
