// A struct representing a simple counter with a name (using a static string slice)
struct NamedCounter {
    name: &'static str,
    count: u32,
    limit: u32,
    enabled: bool,
}

impl NamedCounter {
    // --- Associated Functions (Constructors) ---

    // Primary constructor
    fn new(name: &'static str, limit: u32) -> Self {
        NamedCounter {
            name,
            count: 0,
            limit,
            enabled: true, // Start enabled by default
        }
    }

    // Another constructor for a disabled counter
    fn new_disabled(name: &'static str, limit: u32) -> Self {
        let mut counter = Self::new(name, limit); // Call the primary constructor
        counter.enabled = false;
        counter
    }

    // --- Methods taking &self (Immutable Access) ---

    // Get the current count
    fn get_count(&self) -> u32 {
        self.count
    }

    // Get the name
    fn get_name(&self) -> &'static str {
        self.name
    }

     // Get the limit
    fn get_limit(&self) -> u32 {
        self.limit
    }

    // Check if the counter is enabled
    fn is_enabled(&self) -> bool {
        self.enabled
    }

    // Check if the counter has reached its limit
    fn is_at_limit(&self) -> bool {
        // Example of calling other &self methods
        self.get_count() >= self.get_limit()
    }

    // --- Methods taking &mut self (Mutable Access) ---

    // Increment the counter by 1, respecting the limit and enabled status.
    // Returns true if incremented, false otherwise.
    fn increment(&mut self) -> bool {
        if !self.enabled {
            // Not enabled, cannot increment
            return false;
        }
        if self.is_at_limit() { // Calls &self method `is_at_limit`
            // Already at limit, cannot increment
            return false;
        }

        // Can increment
        self.count += 1;
        true // Return true indicating success
    }

    // Increment the counter by a specific amount. Clamps at the limit.
    // Returns the actual amount the counter was incremented by.
    fn increment_by(&mut self, amount: u32) -> u32 {
         if !self.enabled {
            return 0; // Not enabled, incremented by 0
        }

        let current_count = self.count;
        let potential_count = current_count + amount;

        if potential_count >= self.limit {
             // Clamp to limit
             self.count = self.limit;
             // Return how much was actually added to reach the limit
             self.limit - current_count
        } else {
             // Increase count by the full amount
             self.count = potential_count;
             amount // Full amount was added
        }
    }

    // Reset the counter to zero
    fn reset(&mut self) {
        self.count = 0;
    }

    // Enable the counter
    fn enable(&mut self) {
        self.enabled = true;
    }

    // Disable the counter
    fn disable(&mut self) {
        self.enabled = false;
    }

     // Set a new limit. Clamps count if necessary.
    fn set_limit(&mut self, new_limit: u32) {
        self.limit = new_limit;
        // Clamp count if it now exceeds the new limit
        if self.count > self.limit {
            self.count = self.limit;
        }
    }
} // end impl NamedCounter


fn main() {
    // === Test Case 1: Basic Operations ===
    let mut counter1 = NamedCounter::new("Clicks", 5);

    // Initial state assertions
    assert!(counter1.get_name() == "Clicks");
    assert!(counter1.get_count() == 0);
    assert!(counter1.get_limit() == 5);
    assert!(counter1.is_enabled());
    assert!(!counter1.is_at_limit());

    // Test increment
    assert!(counter1.increment()); // Should succeed (returns true)
    assert!(counter1.get_count() == 1);
    assert!(!counter1.is_at_limit());

    // Test increment_by
    let added = counter1.increment_by(2);
    assert!(added == 2); // Should have added 2
    assert!(counter1.get_count() == 3);
    assert!(!counter1.is_at_limit());

    // Increment to limit
    assert!(counter1.increment()); // count = 4, returns true
    assert!(counter1.get_count() == 4);
    assert!(counter1.increment()); // count = 5 (at limit), returns true
    assert!(counter1.get_count() == 5);
    assert!(counter1.is_at_limit());

    // Try incrementing past limit
    assert!(!counter1.increment()); // Should fail (returns false)
    assert!(counter1.get_count() == 5); // Count should remain 5
    assert!(counter1.is_at_limit());

    // Try increment_by past limit (clamping)
    let added_past_limit = counter1.increment_by(3);
    assert!(added_past_limit == 0); // Added 0 because already at limit 5
    assert!(counter1.get_count() == 5);
    assert!(counter1.is_at_limit());


    // === Test Case 2: Disabling and Enabling ===
    counter1.disable();
    assert!(!counter1.is_enabled());
    assert!(counter1.get_count() == 5); // Count unchanged

    // Try incrementing while disabled
    assert!(!counter1.increment()); // Should fail (returns false)
    assert!(counter1.get_count() == 5);

    // Try increment_by while disabled
    let added_while_disabled = counter1.increment_by(2);
    assert!(added_while_disabled == 0); // Added 0
    assert!(counter1.get_count() == 5);

    // Re-enable
    counter1.enable();
    assert!(counter1.is_enabled());
    assert!(!counter1.increment()); // Still at limit, should fail (returns false)
    assert!(counter1.get_count() == 5);


    // === Test Case 3: Changing Limit ===
    counter1.set_limit(10);
    assert!(counter1.get_limit() == 10);
    assert!(counter1.get_count() == 5); // Count is still 5
    assert!(counter1.is_enabled());
    assert!(!counter1.is_at_limit()); // No longer at limit

    // Increment now that limit is higher
    assert!(counter1.increment()); // count = 6, returns true
    assert!(counter1.get_count() == 6);

    // Increment_by with new limit
    let added_new_limit = counter1.increment_by(3); // 6 + 3 = 9
    assert!(added_new_limit == 3); // Added 3
    assert!(counter1.get_count() == 9);
    assert!(!counter1.is_at_limit());

    // Increment_by that hits the new limit exactly
    let added_to_limit = counter1.increment_by(1); // 9 + 1 = 10
    assert!(added_to_limit == 1); // Added 1
    assert!(counter1.get_count() == 10);
    assert!(counter1.is_at_limit());

    // Increment_by that exceeds new limit (clamping)
    let added_over_limit = counter1.increment_by(5); // Try 10 + 5 -> clamps to 10
    assert!(added_over_limit == 0); // Added 0 because already at limit 10
    assert!(counter1.get_count() == 10);
    assert!(counter1.is_at_limit());

    // Lower the limit below the current count
    counter1.set_limit(7);
    assert!(counter1.get_limit() == 7);
    assert!(counter1.get_count() == 7); // Count should be clamped to new limit
    assert!(counter1.is_at_limit());

    // Try incrementing after clamping
    assert!(!counter1.increment()); // Should fail (at new limit, returns false)
    assert!(counter1.get_count() == 7);


    // === Test Case 4: Resetting ===
    counter1.reset();
    assert!(counter1.get_count() == 0);
    assert!(counter1.get_limit() == 7); // Limit unchanged by reset
    assert!(counter1.is_enabled()); // Enabled status unchanged by reset
    assert!(!counter1.is_at_limit());


    // === Test Case 5: Disabled Constructor ===
    let mut counter2 = NamedCounter::new_disabled("Skips", 100);

    // Initial state assertions for disabled counter
    assert!(counter2.get_name() == "Skips");
    assert!(counter2.get_count() == 0);
    assert!(counter2.get_limit() == 100);
    assert!(!counter2.is_enabled()); // Should be disabled
    assert!(!counter2.is_at_limit());

    // Try incrementing while initially disabled
    assert!(!counter2.increment()); // returns false
    assert!(counter2.get_count() == 0);

    // Enable and increment
    counter2.enable();
    assert!(counter2.is_enabled());
    assert!(counter2.increment()); // returns true
    assert!(counter2.get_count() == 1);


    // === Test Case 6: Edge case with large numbers / saturation ===
    let mut counter3 = NamedCounter::new("OverflowTest", u32::MAX);
    assert!(counter3.get_count() == 0);
    assert!(counter3.get_limit() == u32::MAX);

    // Increment by a large amount, but less than limit
    let added_large = counter3.increment_by(u32::MAX - 10);
    assert!(added_large == u32::MAX - 10);
    assert!(counter3.get_count() == u32::MAX - 10);

    // Increment to exactly the limit
    let added_to_max = counter3.increment_by(10);
    assert!(added_to_max == 10);
    assert!(counter3.get_count() == u32::MAX);
    assert!(counter3.is_at_limit());

    // Try to increment past MAX (should add 0 due to limit check/saturation)
    let added_past_max = counter3.increment_by(5);
    assert!(added_past_max == 0);
    assert!(counter3.get_count() == u32::MAX);

    // If execution reaches here without panicking, all assertions passed.
}