from fusion import fusion 
from extractFragments import extract
import sys


if __name__ == "__main__":
    if len(sys.argv) <4:
        print "python fusion.py input_BU input_TD groundtruth"
        exit(1)
    else:
        input_BU = sys.args[1]
        input_TD = sys.args[2]
        groundtruth = sys.args[3]
#        input_BU can stay as it is
        
#        Patterns from input_TD have to be extracted and mapped according to the groundtruth
        extract("bla","extracted")
        
#        evaluation perl .pl ground input_BU 1 1
#        evaluation perl .pl ground extracted 1 1
        
#        create fusion
        fusion(input_BU,input_TD,"fusion_result")
#        evaluation perl .pl ground fusion 1 1



        
        
        
    