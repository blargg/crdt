crdt
==========

[Conflict-free Replicated Data Type]( https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type) implimented in Haskell.
A CRDT keeps data synchonized across different computers in a distributed system. Each computer can modify the data simultaneously, and the system will eventually reconcile all the computers to agree on the same value.

`src-exe/Main.hs` is an example use case. It keeps a time ordered conversation.
Time is ordered by Lamport Timestamps. Each computer in the system modifies their own version of the conversation, and the crdt syncs the conversations together.
