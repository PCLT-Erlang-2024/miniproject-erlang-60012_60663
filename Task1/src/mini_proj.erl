-module(mini_proj).
-export([start/1, stop/0]).

%%start with a specified number of conveyors, each conveyor has its "line" of trucks
start(NumConveyors) when is_integer(NumConveyors), NumConveyors > 0 ->
    
    ConveyorIds = lists:seq(1, NumConveyors),
    lists:foreach(fun(ConveyorId) ->
        ConveyorPid = start_conveyor(ConveyorId),
        spawn(fun() -> package_generator(ConveyorId, ConveyorPid, 1) end)
    end, ConveyorIds),
    ok.


package_generator(ConveyorId, ConveyorPid, PackageNumber) ->
    %%simulate package creation
    PackageId = list_to_atom(integer_to_list(ConveyorId) ++ "-" ++ integer_to_list(PackageNumber)),
    Package = {package, PackageId},
    %%send package to conveyor belt
    ConveyorPid ! {new_package, Package},
    timer:sleep(500), %%simulate time between packages
    package_generator(ConveyorId, ConveyorPid, PackageNumber + 1).


start_conveyor(ConveyorId) ->
    Name = list_to_atom("conveyor_" ++ integer_to_list(ConveyorId)),
    ConveyorPid = spawn(fun() -> conveyor_loop(ConveyorId, 1, start_truck(ConveyorId, 1)) end),
    register(Name, ConveyorPid),
    ConveyorPid.

conveyor_loop(ConveyorId, TruckNumber, TruckPid) ->
    receive
        {new_package, Package} ->
            io:format("Conveyor ~p received ~p~n", [ConveyorId, Package]),
            %%send the package to the current truck
            TruckPid ! {load_package, Package, self()},
            conveyor_loop(ConveyorId, TruckNumber, TruckPid);

        {truck_full, OldTruckPid} ->
            %%start a new truck
            NewTruckNumber = TruckNumber + 1,
            NewTruckPid = start_truck(ConveyorId, NewTruckNumber),
            conveyor_loop(ConveyorId, NewTruckNumber, NewTruckPid);

        stop ->
            io:format("Conveyor ~p stopping.~n", [ConveyorId]),
            TruckPid ! stop;  %%stop current truck

        _Other ->
            conveyor_loop(ConveyorId, TruckNumber, TruckPid)
    end.


start_truck(ConveyorId, TruckNumber) ->
    Name = list_to_atom("truck_" ++ integer_to_list(ConveyorId) ++ "_" ++ integer_to_list(TruckNumber)),
    TruckPid = spawn(fun() -> truck_loop(ConveyorId, TruckNumber, [], 5) end),
    register(Name, TruckPid),
    TruckPid.

truck_loop(ConveyorId, TruckNumber, Packages, Capacity) ->
    receive
        {load_package, Package, ConveyorPid} when length(Packages) < Capacity - 1 ->
            io:format("Truck ~p-~p loaded ~p~n", [ConveyorId, TruckNumber, Package]),
            truck_loop(ConveyorId, TruckNumber, [Package | Packages], Capacity);

        {load_package, Package, ConveyorPid} ->
            %%truck is full after this package
            io:format("Truck ~p-~p loaded ~p and is now full. Dispatching...~n", [ConveyorId, TruckNumber, Package]),
            dispatch_truck(ConveyorId, TruckNumber, [Package | Packages]),
            %%tell the conveyor that the truck is full
            ConveyorPid ! {truck_full, self()},
            unregister(list_to_atom("truck_" ++ integer_to_list(ConveyorId) ++ "_" ++ integer_to_list(TruckNumber))),
            truck_loop(ConveyorId, TruckNumber, [], Capacity);

        stop ->
            io:format("Truck ~p-~p stopping.~n", [ConveyorId, TruckNumber]),
            unregister(list_to_atom("truck_" ++ integer_to_list(ConveyorId) ++ "_" ++ integer_to_list(TruckNumber)));

        _Other ->
            truck_loop(ConveyorId, TruckNumber, Packages, Capacity)
    end.

dispatch_truck(ConveyorId, TruckNumber, Packages) ->
    io:format("Truck ~p-~p dispatched with packages: ~p~n", [ConveyorId, TruckNumber, lists:reverse(Packages)]).


stop() ->
    %%get all registered processes
    Registered = registered(),
    %%send stop messages
    lists:foreach(fun(Name) ->
        case atom_to_list(Name) of
            "conveyor_" ++ _Rest ->
                whereis(Name) ! stop;
            _Other ->
                ok
        end
    end, Registered),
    ok.
