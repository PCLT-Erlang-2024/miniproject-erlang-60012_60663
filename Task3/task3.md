## Task 3 - Delays for replacing trucks
By simply adding a sleep in the conveyor loop function we can obtain the delay between truck replacement.
The truck process detects that it has reached its capacity, the truck sends a message to the conveyor process, notifying it that the truck is full. 
Conveyor Receives {truck_full, OldTruckPid}, the conveyor’s receive block matches the {truck_full, OldTruckPid} message.
It processes this message by printing a message indicating it’s waiting for a new truck and by calling timer:sleep/1000 to simulate a delay for truck replacement.