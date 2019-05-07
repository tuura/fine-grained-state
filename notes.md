## How do we address the challenges from "A Survey of Symbolic Execution Techniques"

### Memory

> How does the symbolic engine handle pointers, arrays, or other complex objects?
> Code manipulating pointers and data structures may give rise not only to symbolic stored
> data, but also to addresses being described by symbolic expressions.

The instruction set has a `loadMI` instruction, which performs a memory-indirect data load,
i.e. it populates a register with a value addressed by another another value stored in a
certain memory address.

At that point, we do not allow symbolic pointers. We only allow the pointer value to be either a literal constant or an expression reducible to a such a constant. If the expression contains a
symbolic variable, our model would not stop the execution and report it.

### Environment

> How does the engine handle interactions across the software stack? Calls to
> library and system code can cause side effects, e.g., the creation of a file or a call
> back to user code, that could later affect the execution and must be accounted for.
> However, evaluating any possible interaction outcome may be unfeasible.

### State space explosion

> How does symbolic execution deal with path explosion? Language
> constructs such as loops might exponentially increase the number of execution states. It
> is thus unlikely that a symbolic execution engine can exhaustively explore all the possible
> states within a reasonable amount of time.

### Constraint solving

> What can a constraint solver do in practice? SMT solvers can scale to
> complex combinations of constraints over hundreds of variables. However, constructs such
> as non-linear arithmetic pose a major obstacle to efficiency.


## References

