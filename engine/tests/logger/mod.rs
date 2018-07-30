extern crate slog_term;

use slog::Drain;

pub use slog::Logger;

pub fn logger() -> Logger {
    let plain = slog_term::PlainSyncDecorator::new(::std::io::stdout());
    Logger::root(slog_term::FullFormat::new(plain).build().fuse(), o!())
}
