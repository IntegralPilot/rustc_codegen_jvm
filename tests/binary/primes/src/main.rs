fn is_prime(n: u32) -> bool {
    if n < 2 {
        return false;
    }
    if n == 2 || n == 3 {
        return true;
    }
    if n % 2 == 0 {
        return false;
    }

    let mut i = 3;
    while i * i <= n {
        if n % i == 0 {
            return false;
        }
        i += 2;
    }
    true
}

fn find_primes(start: u32, count: usize, buffer: &mut [u32]) -> usize {
    let mut found = 0;
    let mut candidate = if start % 2 == 0 { start + 1 } else { start };

    while found < count && found < buffer.len() {
        if is_prime(candidate) {
            buffer[found] = candidate;
            found += 1;
        }
        candidate += 2;
    }

    found
}

fn main() {
    let mut primes = [0u32; 10];
    let found = find_primes(10_000, 10, &mut primes);

    let mut i: usize = 0;
    while i < found {
        assert!(is_prime(primes[i]));
        i += 1;
    }
}