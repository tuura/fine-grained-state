# Find the greatest common divisor of values in memory locations 0 and 1,
# put result to the register R1
IF (Set R0 0)
IF (Store R0 255)
IF (Load R0 1)
# Test register R0 for being zero by subtracting zero
IA (Sub R0 255)
# Halt if register R0 contains zero, loop otherwise
IS (JumpZero 6)
IF (Load R0 0)
IA (Mod R0 1)
IF (Load R1 1)
IF (Store R0 1)
IF (Store R1 0)
IF (Jump (-8))
IF Halt