#!/usr/bin/env python
import json
import sys
from pprint import pprint
import pydot
import subprocess
import time


# recursively find all the direct calls given the original dict, which we know is a function
def find_calls(json_dict, func_calls):
  if "Type" in json_dict:
    if json_dict["Type"] == "Call":
      if json_dict["Call"]["Calltype"] == "direct": 
        func_calls.append(json_dict["Call"]["Callee"])
     #   # extract arguments
     #   args_list = json_dict["Call"]["Args"]
     #   type_name_list = []
     #   for arg in args_list:
     #     # type is 
     #     t = arg["Type"]
     #     # arg is 
     #     a = arg["Arg"]
     #     type_name_list.append((t,a))
           
  for key in json_dict:
    if type(json_dict[key]) == dict:
      find_calls(json_dict[key], func_calls)
    elif type(json_dict[key]) == list:
      for entry in json_dict[key]:
        if type(entry) == dict:
          find_calls(entry, func_calls)
  
# recursive dfs to find transitive calls
def cg_dfs(callees, x, visited, call_graph_as_dict):
  for node in callees:
    if node == x:
      return True
    elif node not in set(visited):
      visited.append(node)
      try:
        # might not be a declared function, e.g. printf
        cg_dfs(call_graph_as_dict[node], x, visited, call_graph_as_dict)
      except:
        continue
 
def main():
  # declare some random shit here
  if len(sys.argv) < 2:
    print "Usage: ./analyze [JSON_FILENAME]\n"
    print "Please provide a program as json data"

  # internal data structure for json
  json_structures = []
  calls = {}
  global_vars = {}
  with open(sys.argv[1], 'r') as code_data:
    # each line in the file is a seperate json data object
    for line in code_data.readlines():
      # load takes a string, converts the json into a dict
      data = json.loads(line)
      #pprint(data)
      
      # if function, then we pass to find calls, in order to compute call graph
      if "Type" in data:
        if data["Type"] == "Function":
          caller = data["Name"]
          calls[caller] = []
          find_calls(data, calls[caller])
        elif data["Type"] == "Var":
          var_dict = data["Var"]
          name = var_dict["Name"]
          vartype = var_dict["Type"]
          global_vars[vartype + " " + name] = "Unitialized"
          if "Init" in var_dict:
            global_vars[vartype + " " + name] = "Initialized"
                 
      if type(data) == dict:
        json_structures.append(data)
  #pprint(calls)
  # create the call graph
  graph = pydot.Dot(graph_type='digraph')
  
  for node_a in calls:
    for node_b in calls[node_a]:
      edge = pydot.Edge(node_a, node_b)
      graph.add_edge(edge) 
  
  print "Current static analyses includes: " 
  print "   - reachability"
  print "   - show callgraph" 
  print "   - globals" 
  print "Type 'quit' to exit the analyzer"

  while 1:
    str_input = raw_input('\nAnalysis to run: ')
    if str_input == 'reachability':
      node1 = raw_input('caller: ')
      node2 = raw_input('callee: ')
      if node1 in calls and node2 in calls:
        if cg_dfs(calls[node1], node2, [node1], calls):
          print node1 + "->" + node2 + " is reachable"
        else:
          print node1 + "->" + node2 + " is unreachable"
      else:
        print "\nSelected unknown function name, please try again"
        print "Possible functions include:"
        for key in calls:
          print "   - " + key
    elif str_input == 'show callgraph':
        graph.write_png('callgraph.png')
        try:
          subprocess.call('open callgraph.png', shell=True)
          subprocess.call('eog callgraph.png', shell=True)
        except:
          print "Callgraph stored as temp_callgraph.png"
    elif str_input == 'globals':
        pprint(global_vars) 
    elif str_input == 'quit':
      sys.exit()
    else:
      print "command not recognized, please try again\n"
      print "Current static analyses includes: " 
      print "   - reachability"
      print "   - show callgraph" 
      print "   - globals" 
      print "Type 'quit' to exit the analyzer"

  return 0








if __name__ == "__main__":
  main()
