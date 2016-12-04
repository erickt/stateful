#[macro_export]
macro_rules! span_warn {
    ($cx:expr, $span:expr, $($message:tt)*) => ({
        $cx.span_warn(
            $span,
            &format!("{}:{}: {}", file!(), line!(), format_args!($($message)*)))
    })
}

#[macro_export]
macro_rules! span_err {
    ($cx:expr, $span:expr, $($message:tt)*) => ({
        $cx.span_err(
            $span,
            &format!("{}:{}: {}", file!(), line!(), format_args!($($message)*)))
    })
}

#[macro_export]
macro_rules! span_bug {
    ($cx:expr, $span:expr, $($message:tt)*) => ({
        $cx.span_bug(
            $span,
            &format!("{}:{}: {}", file!(), line!(), format_args!($($message)*)))
    })
}
