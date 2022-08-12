
export const ENV_SUPPORTS_NATIVE_BIGINT = (function() { try { new Function("BigInt")(); return 1; } catch (e) { return 0; } })()

