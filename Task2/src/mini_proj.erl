-module(mini_proj).
-export([start/1, stop/0]).

start(NumConveyors) when is_integer(NumConveyors), NumConveyors > 0 ->
    
    ConveyorIds = lists:seq(1, NumConveyors),
    lists:foreach(fun(ConveyorId) ->
        ConveyorPid = start_conveyor(ConveyorId),
        spawn(fun() -> package_generator(ConveyorId, ConveyorPid, 1) end)
    end, ConveyorIds),
    ok.

package_generator(ConveyorId, ConveyorPid, PackageNumber) ->
    %%generate a random package size between 1 and 3
    PackageSize = rand:uniform(3),
    PackageId = list_to_atom(integer_to_list(ConveyorId) ++ "-" ++ integer_to_list(PackageNumber)),
    Package = {package, PackageId, PackageSize},
    ConveyorPid ! {new_package, Package},
    timer:sleep(500),
    package_generator(ConveyorId, ConveyorPid, PackageNumber + 1).

start_conveyor(ConveyorId) ->
    Name = list_to_atom("conveyor_" ++ integer_to_list(ConveyorId)),

    InitialCapacity = 5,
    ConveyorPid = spawn(fun() -> conveyor_loop(ConveyorId, 1, start_truck(ConveyorId, 1, InitialCapacity), InitialCapacity) end),
    register(Name, ConveyorPid),
    ConveyorPid.

conveyor_loop(ConveyorId, TruckNumber, TruckPid, TruckCapacity) ->
    receive
        {new_package, Package} ->
            io:format("Conveyor ~p received ~p~n", [ConveyorId, Package]),
            TruckPid ! {load_package, Package, self()},
            conveyor_loop(ConveyorId, TruckNumber, TruckPid, TruckCapacity);

        {truck_full, OldTruckPid, Package} ->
            NewTruckNumber = TruckNumber + 1,
            %%reset truck capacity
            InitialCapacity = 5,
            NewTruckPid = start_truck(ConveyorId, NewTruckNumber, InitialCapacity),
            %%try loading the package onto the new truck
            NewTruckPid ! {load_package, Package, self()},
            conveyor_loop(ConveyorId, NewTruckNumber, NewTruckPid, InitialCapacity);

        stop ->
            io:format("Conveyor ~p stopping.~n", [ConveyorId]),
            TruckPid ! stop;  

        _Other ->
            conveyor_loop(ConveyorId, TruckNumber, TruckPid, TruckCapacity)
    end.


start_truck(ConveyorId, TruckNumber, Capacity) ->
    Name = list_to_atom("truck_" ++ integer_to_list(ConveyorId) ++ "_" ++ integer_to_list(TruckNumber)),
    TruckPid = spawn(fun() -> truck_loop(ConveyorId, TruckNumber, [], Capacity, Capacity) end),
    register(Name, TruckPid),
    TruckPid.

truck_loop(ConveyorId, TruckNumber, Packages, Capacity, RemainingCapacity) ->
    receive
        {load_package, Package, ConveyorPid} ->
            {package, PackageId, PackageSize} = Package,
            if
                PackageSize =< RemainingCapacity ->
                    %%load the package
                    NewRemainingCapacity = RemainingCapacity - PackageSize,
                    io:format("Truck ~p-~p loaded ~p (Size: ~p). Remaining Capacity: ~p~n",
                              [ConveyorId, TruckNumber, PackageId, PackageSize, NewRemainingCapacity]),
                    truck_loop(ConveyorId, TruckNumber, [Package | Packages], Capacity, NewRemainingCapacity);
                true ->
                    %%not enough capacity, dispatch and replace
                    io:format("Truck ~p-~p cannot load ~p (Size: ~p). Not enough capacity.~n",
                              [ConveyorId, TruckNumber, PackageId, PackageSize]),
                    dispatch_truck(ConveyorId, TruckNumber, Packages),
                    %%inform the conveyor that the truck is full and pass back the package
                    ConveyorPid ! {truck_full, self(), Package},
                    unregister(list_to_atom("truck_" ++ integer_to_list(ConveyorId) ++ "_" ++ integer_to_list(TruckNumber))),
                    exit(normal)
            end;

        stop ->
            io:format("Truck ~p-~p stopping.~n", [ConveyorId, TruckNumber]),
            unregister(list_to_atom("truck_" ++ integer_to_list(ConveyorId) ++ "_" ++ integer_to_list(TruckNumber))),
            exit(normal);

        _Other ->
            truck_loop(ConveyorId, TruckNumber, Packages, Capacity, RemainingCapacity)
    end.


dispatch_truck(ConveyorId, TruckNumber, Packages) ->
    PackageInfo = [{PackageId, Size} || {package, PackageId, Size} <- lists:reverse(Packages)],
    io:format("Truck ~p-~p dispatched with packages: ~p~n", [ConveyorId, TruckNumber, PackageInfo]).


stop() ->
    Registered = registered(),
    lists:foreach(fun(Name) ->
        case atom_to_list(Name) of
            "conveyor_" ++ _Rest ->
                whereis(Name) ! stop;
            _Other ->
                ok
        end
    end, Registered),
    ok.
