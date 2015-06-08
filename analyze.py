#!/usr/bin/env python
import json
import sys
from pprint import pprint
ispydot = True
try:
  import pydot
  ispydot = True
except:
  print "WARNING - you do not have pydot installed, this is needed for the callgraph"
  ispydot = False
import subprocess


# recursively find all the direct calls given the original dict, which we know is a function
def find_calls(json_dict, func_calls):
  if "Type" in json_dict:
    if json_dict["Type"] == "Call":
      if json_dict["Call"]["Calltype"] == "direct": 
        func_calls.append(str(json_dict["Call"]["Callee"]))
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

# recursively find function calls that do not get assigned to a variable 
def find_unassigned(json_dict, calls):
  for key in json_dict:
    if type(json_dict[key]) == dict:
      if "Call" in json_dict[key]:
        if json_dict[key]["Call"]["Calltype"] == "direct":
          if key == "Rhs":
            continue
          else:
            calls.append(str(json_dict[key]["Call"]["Callee"]) + " is not assigned to a variable") 
    if type(json_dict[key]) == dict:
      find_unassigned(json_dict[key], calls)
    elif type(json_dict[key]) == list:
      for entry in json_dict[key]:
        if type(entry) == dict:
          find_unassigned(entry, calls)

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

#recursive dfs to find all modified variables in a function
def ms_dfs(var_list, json_dict):
  if "Var" in json_dict:
    var_dict = json_dict["Var"]
    if "Init" in var_dict:
      if var_dict["Name"] not in var_list:
        var_list.append(str(var_dict["Name"]))
  for key in json_dict:
    if type(json_dict[key]) == dict:
      ms_dfs(var_list, json_dict[key])
    elif type(json_dict[key]) == list:
      for entry in json_dict[key]:
        if type(entry) == dict:
          ms_dfs(var_list, entry)
      
 
def main():
  # declare some random shit here
  if len(sys.argv) < 2:
    print "Usage: ./analyze [JSON_FILENAME]\n"
    print "Please provide a program as json data"

  # internal data structure for json
  json_structures = []
  calls = {}
  global_vars = {}
  mod_set = {}
  unassigned = {}
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
          calls[str(caller)] = []
          mod_set[str(caller)] = []
          find_calls(data, calls[caller])
          ms_dfs(mod_set[caller], data)
          unassigned[str(caller)] = [] 
          find_unassigned(data, unassigned[caller])
        elif data["Type"] == "Var":
          var_dict = data["Var"]
          name = var_dict["Name"]
          vartype = var_dict["Type"]
          global_vars[str(vartype) + " " + str(name)] = "Unitialized"
          if "Init" in var_dict:
            global_vars[str(vartype) + " " + str(name)] = "Initialized"
                 
      if type(data) == dict:
        json_structures.append(data)

  # create the call graph
  if ispydot:
    graph = pydot.Dot(graph_type='digraph')
    
    for node_a in calls:
      for node_b in calls[node_a]:
        edge = pydot.Edge(node_a, node_b)
        graph.add_edge(edge) 
    
  print "\nCurrent static analyses includes: " 
  print "   - reachability"
  print "   - show callgraph" 
  print "   - globals" 
  print "   - mod set"
  print "   - global functions"
  print "   - unassigned returns"
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
      if ispydot: 
        graph.write_png('callgraph.png')
        try:
          subprocess.call('open callgraph.png', shell=True)
          subprocess.call('eog callgraph.png', shell=True)
        except:
          print "Callgraph stored as callgraph.png"
      else:
        print "you do not have pydot installed, please do not use this command"
    elif str_input == 'globals':
        print "\n Global variables are:"
        pprint(global_vars) 
    elif str_input == 'mod set':
      print "\n The mod set for the functions are: "  
      pprint(mod_set)
    elif str_input == 'global functions':
      print "\nThe follow functions are able to modify global variables:"
      temp = []
      temp2 = []
      for key in global_vars:
        x = key.split(" ")
        temp.append(x[1])
      for key in mod_set:
        for node in mod_set[key]:
          if node in temp:
            temp2.append(key)
      a = set(temp2)
      pprint(a)
    elif str_input == 'unassigned returns':
      pprint(unassigned) 
    elif str_input == 'quit':
      sys.exit()
    else:
      print "\ncommand not recognized, please try again\n"
      print "Current static analyses includes: " 
      print "   - reachability"
      print "   - show callgraph" 
      print "   - globals" 
      print "   - mod set"
      print "   - global functions"
      print "   - unassigned returns"
      print "Type 'quit' to exit the analyzer"

  return 0








if __name__ == "__main__":
  main()
