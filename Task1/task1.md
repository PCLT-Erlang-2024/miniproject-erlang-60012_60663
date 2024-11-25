# Correctness Properties and Their Implementation

## Concurrency
Each conveyor and truck is implemented as an independent Erlang process. The start/1 function initializes multiple conveyors and their associated trucks based on the specified number, allowing them to run concurrently.

## Deadlock-Free Operation
Processes communicate through non-blocking message passing. The recursive receive loops in the conveyor and truck processes handle messages promptly, preventing any process from waiting indefinitely.

## Progress Guarantee
Package generators continuously create packages and send them to conveyors. Conveyors and trucks recursively process incoming packages and messages, ensuring ongoing operation without stalls.

## Logical Order of Events
The system enforces this sequence through the flow of messages:
Packages are created by package_generator/3.
Sent to conveyors via {new_package, Package} messages.
Conveyors forward packages to trucks using {load_package, Package, ConveyorPid} messages.
Trucks load packages and, upon reaching capacity, dispatch and notify conveyors to start new trucks.

## Message Passing Coordination
Erlang's message passing mechanisms (! and receive) are exclusively used for inter-process communication. This ensures loose coupling and synchronization between processes.