# -*- coding: utf-8 -*-
"""
Created on Fri Jun 26 13:01:30 2020
@author: Kiri Daust
"""

"""Vehicle Routing Problem for PEM sampling"""

from ortools.constraint_solver import routing_enums_pb2
from ortools.constraint_solver import pywrapcp
import numpy as np

def py_mTSP(dat, num_days, start, end, max_cost, plot_time, penalty, arbDepot, GSC = 10):
    dat2 = dat.copy()
    # if(arbDepot):
    #     temp = start.append(max(start)+1)
    #     dat2[temp,] = 0
    #     dat2[:,temp] = 0
    data = {}
    data['distance_matrix'] = dat2
    data['num_vehicles'] = num_days
    data['starts'] = start
    data['ends'] = end
    
    # Create the routing index manager.
    manager = pywrapcp.RoutingIndexManager(len(data['distance_matrix']),data['num_vehicles'], data['starts'], data['ends'])
    # Create Routing Model.
    routing = pywrapcp.RoutingModel(manager)

    # Create and register a transit callback.
    def distance_callback(from_index, to_index):
        """Returns the distance between the two nodes."""
        # Convert from routing variable Index to distance matrix NodeIndex.
        from_node = manager.IndexToNode(from_index)
        to_node = manager.IndexToNode(to_index)
        if(to_node in end): ##need to fix this part
            add = 0
        else:
            add = plot_time
        return data['distance_matrix'][from_node][to_node]+add

    transit_callback_index = routing.RegisterTransitCallback(distance_callback)

    # Define cost of each arc.
    routing.SetArcCostEvaluatorOfAllVehicles(transit_callback_index)

    # Add Distance constraint.
    dimension_name = 'Distance'
    routing.AddDimension(
        transit_callback_index,
        0,  # no slack
        max_cost,  # vehicle maximum travel distance
        True,  # start cumul to zero
        dimension_name)
        
    # Allow to drop nodes.
    for node in range(len(penalty)):
        routing.AddDisjunction([manager.NodeToIndex(node)], penalty[node])

    distance_dimension = routing.GetDimensionOrDie(dimension_name)

    distance_dimension.SetGlobalSpanCostCoefficient(GSC)

    search_parameters = pywrapcp.DefaultRoutingSearchParameters()
    search_parameters.time_limit.seconds = 30
    # Setting first solution heuristic.
    search_parameters = pywrapcp.DefaultRoutingSearchParameters()
    search_parameters.first_solution_strategy = (
        routing_enums_pb2.FirstSolutionStrategy.PATH_CHEAPEST_ARC)
    # Solve the problem.
    solution = routing.SolveWithParameters(search_parameters)
    
    ##collect solution and return
    plan_output = dict.fromkeys(range(data['num_vehicles']),None)
    dist_output = dict.fromkeys(range(data['num_vehicles']),None)
    for vehicle_id in range(data['num_vehicles']):
        index = routing.Start(vehicle_id)
        temp = []
        rd = 0
        while not routing.IsEnd(index):
            temp.append(manager.IndexToNode(index))
            previous_index = index
            index = solution.Value(routing.NextVar(index))
            rd += routing.GetArcCostForVehicle(
                previous_index, index, vehicle_id)
        temp.append(manager.IndexToNode(index))
        if(arbDepot):
            temp = temp[:-1]
        plan_output[vehicle_id] = temp
        dist_output[vehicle_id] = rd
    
    return(plan_output,dist_output)
