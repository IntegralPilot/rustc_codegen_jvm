package org.rustlang.runtime;

/** Exposes the concrete payload hidden by a generated Rust trait-object adapter. */
public interface TraitObjectCarrier {
    Object rustTraitObjectPayload();
}
