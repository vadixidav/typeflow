pub fn log() {
    env_logger::builder().is_test(true).try_init().ok();
}