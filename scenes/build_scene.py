import json
import sys
import os.path

try:
    scene_filename = sys.argv[1]
    with open(scene_filename) as f:
        scene_file = json.load(f)
except IndexError:
    raise ValueError('no file name given')

stls = {}

def read_stl(path):
    if path in stls:
        return stls[path]
    (normals,facets) = [],[]
    with open(path) as stl_file:
        stl_lines = stl_file.readlines()
        for i in range(1,len(stl_lines)-1,7):
            facet = stl_lines[i:i+7]
            (normal,v1,v2,v3) = [ list(map(float,facet[i].split()[-3:])) for i in [0,2,3,4] ]
            normals.append(normal)
            facets.append([v1,v2,v3])
    stl = (normals,facets)
    stls[path] = stl
    return stl

def show_v3(v):
    # TODO: use scientific notation?
    return ' '.join(map(str,v))

def indent(strs):
    return [ '\t'+s for s in strs ]

def write_stl(stl,name,path):
    with open(path, 'w') as stl_file:
        stl_file.write(f'solid "{name}"\n')
        for (normal,facet) in stl:
            stl_file.write(
                '\n'.join(indent(
                    [ f'facet normal {show_v3(normal)}']
                    + indent(
                        ['outer loop']
                        + indent([ f'vertex {show_v3(vertex)}' for vertex in facet ]) +
                        ['endloop']
                    ) +
                    [f'endfacet']
                )) + '\n'
            )
        stl_file.write(f'endsolid "{name}"\n')

scene_stl = []

for block in scene_file:
    (normals,facets) = read_stl(block['file'])
    offset = list(map(float,block['offset']))
    new_facets = [ [ [ v+o for (v,o) in zip(vertex,offset) ] for vertex in facet] for facet in facets ]
    scene_stl.extend(zip(normals,new_facets))

write_stl(scene_stl, scene_filename, os.path.splitext(scene_filename)[0] + '.stl')