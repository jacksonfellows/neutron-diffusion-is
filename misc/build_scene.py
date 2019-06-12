import json
import os.path
import sys

from STL import STL

scene_filename = sys.argv[1]
scene_basename = os.path.basename(scene_filename)
scene_dir = os.path.dirname(scene_filename)
scene_pairs = []
with open(scene_filename) as scene_file:
    scene = json.load(scene_file)
    for obj in scene:
        stl = STL(os.path.join(scene_dir, obj['file']))
        stl.vertices += obj['offset']
        scene_pairs.extend(zip(stl.normals,stl.facets))

scene_stl = STL(pair=zip(*scene_pairs))
scene_stl.write(f'{os.path.splitext(scene_basename)[0]}.stl')

