crdt
==========

[Conflict-free Replicated Data Type]( https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type) implimented in Haskell.
A CRDT keeps data synchonized across different processes in a distributed system. Each process can modify the data simultaneously, and the system will eventually reconcile all the processes to agree on the same value.

`src-exe/Main.hs` is an example use case. It keeps a time ordered conversation.
Time is ordered by Lamport Timestamps. Each process in the system modifies their own version of the conversation, and the CRDT syncs the conversations together.

Building
--------
The example can be built and run with the following.
```bash
stack install
crdt-exe --port 8888 "localhost:9999"
```
This starts the program, which listens on port 8888 and expects communication to `localhost:9999`. In another prompt, start the corresponding program with
```bash
crdt-exe --port 9999 "localhost:8888"
```

This will open a basic text prompt.
Entering the string "show" will output the current value of the CRDT
Entering any other string will add that string at the current time
using [lamport timestamps](https://en.wikipedia.org/wiki/Lamport_timestamps)

See the crdt-exe help text for more information
```bash
crdt-exe --help
```

Algorithm
---------
This implementation is a delta state CRDT, which improves efficiency by only sending deltas of changes made.

A basic CRDT is a [semilattice]( https://en.wikipedia.org/wiki/Semilattice)

This implementation is of a Delta State CRDT. This improves effeciency by only transmitting deltas of the changes made.
This version uses a Semilattice Action. Let `delta` and `state` be types. Then a Semilattice Action of `delta` acting on `state` with action `apply:delta -> state -> state` obeys the following laws

* `delta` is a Semilattice
* `forall d1,d2:delta s:state. apply (d1 \/ d2) s = apply (d1 (apply d2 s))`

where `\/` is the join operation for `delta`.

The state can be modified by applying deltas. The deltas can then be transmitted to the other processes in the system.

With this information, we can show that when all processes have at least one copy of every delta, the state after applying them is the same. This doesn't depend on the order in which the deltas are applied.
