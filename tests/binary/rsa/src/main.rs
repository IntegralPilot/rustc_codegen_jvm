struct PublicKey {
    e: u64,
    n: u64,
}

struct PrivateKey {
    d: u64,
    n: u64,
}

// Modular exponentiation: (base^exp) % modulus
fn mod_exp(mut base: u64, mut exp: u64, modulus: u64) -> u64 {
    let mut result = 1;
    base %= modulus;

    while exp > 0 {
        if exp % 2 == 1 {
            result = (result * base) % modulus;
        }
        exp >>= 1;
        base = (base * base) % modulus;
    }

    result
}

// Extended Euclidean Algorithm to find modular inverse
fn mod_inv(a: i64, m: i64) -> i64 {
    let (mut m0, mut x0, mut x1) = (m, 0, 1);
    let mut a = a;

    while a > 1 {
        let q = a / m0;
        let t = m0;
        m0 = a % m0;
        a = t;
        let t = x0;
        x0 = x1 - q * x0;
        x1 = t;
    }

    if x1 < 0 {
        x1 += m;
    }

    x1
}

// RSA key generation (with hardcoded small primes for simplicity)
fn generate_keys() -> (PublicKey, PrivateKey) {
    let p = 61;
    let q = 53;
    let n = p * q;
    let phi = (p - 1) * (q - 1);
    let e = 17;
    let d = mod_inv(e as i64, phi as i64) as u64;

    (
        PublicKey { e, n },
        PrivateKey { d, n },
    )
}

// RSA encryption: c = m^e mod n
fn encrypt(pub_key: &PublicKey, message: u64) -> u64 {
    mod_exp(message, pub_key.e, pub_key.n)
}

// RSA decryption: m = c^d mod n
fn decrypt(priv_key: &PrivateKey, ciphertext: u64) -> u64 {
    mod_exp(ciphertext, priv_key.d, priv_key.n)
}

// Main test function
fn main() {
    let (public_key, private_key) = generate_keys();

    // Test message
    let message: u64 = 42;
    assert!(message < public_key.n);

    let encrypted = encrypt(&public_key, message);
    let decrypted = decrypt(&private_key, encrypted);

    // Check correctness
    assert!(decrypted == message);
    assert!(encrypted != message); // Make sure encryption changes the message
}