//! Deterministic RNG helpers for bellman-based proof systems.

use rand::rngs::ThreadRng;
use rand::{CryptoRng, Error, RngCore, SeedableRng};
use rand_chacha::ChaCha20Rng;

/// RNG wrapper used by bellman setup/proving.
#[derive(Debug)]
pub enum BellmanRng {
    /// Use the OS-backed thread RNG (default).
    Thread(ThreadRng),
    /// Use a deterministic ChaCha RNG seeded from `CIRC_BELLMAN_SEED`.
    ChaCha(ChaCha20Rng),
}

impl RngCore for BellmanRng {
    fn next_u32(&mut self) -> u32 {
        match self {
            BellmanRng::Thread(rng) => rng.next_u32(),
            BellmanRng::ChaCha(rng) => rng.next_u32(),
        }
    }

    fn next_u64(&mut self) -> u64 {
        match self {
            BellmanRng::Thread(rng) => rng.next_u64(),
            BellmanRng::ChaCha(rng) => rng.next_u64(),
        }
    }

    fn fill_bytes(&mut self, dest: &mut [u8]) {
        match self {
            BellmanRng::Thread(rng) => rng.fill_bytes(dest),
            BellmanRng::ChaCha(rng) => rng.fill_bytes(dest),
        }
    }

    fn try_fill_bytes(&mut self, dest: &mut [u8]) -> Result<(), Error> {
        match self {
            BellmanRng::Thread(rng) => rng.try_fill_bytes(dest),
            BellmanRng::ChaCha(rng) => rng.try_fill_bytes(dest),
        }
    }
}

impl CryptoRng for BellmanRng {}

/// Returns a bellman-compatible RNG.
///
/// If `CIRC_BELLMAN_SEED` is set to a non-empty `u64`, use a deterministic
/// ChaCha20 RNG. Otherwise, fall back to the thread RNG.
pub fn bellman_rng() -> BellmanRng {
    match std::env::var("CIRC_BELLMAN_SEED") {
        Ok(seed_str) if !seed_str.trim().is_empty() => {
            let seed = seed_str
                .trim()
                .parse::<u64>()
                .expect("CIRC_BELLMAN_SEED must be a u64");
            let mut bytes = [0u8; 32];
            bytes[..8].copy_from_slice(&seed.to_le_bytes());
            BellmanRng::ChaCha(ChaCha20Rng::from_seed(bytes))
        }
        _ => BellmanRng::Thread(rand::thread_rng()),
    }
}
