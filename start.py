from fusion import start_fusion as fusion 
from extractFragments import extractFromFile as extract
import sys, subprocess

argument1 = str(1)
argument2 = str(1)
#0 uses exact match
#1 uses Levenshtein


if __name__ == "__main__":
    if len(sys.argv) <4:
        print "python start.py input_BU input_TD groundtruth"
        exit(1)
    else:
        input_BU = sys.argv[1]
        input_TD = sys.argv[2]
        groundtruth = sys.argv[3]
#        input_BU can stay as it is

#        Patterns from input_TD have to be extracted and mapped according to the groundtruth
        extract(input_TD,"extracted",groundtruth)
        print "Extraction and mapping done"
        

        
#        create fusion
#        fusion(input_BU,"extracted","fusion_result")
        fusion(input_BU,"extracted","fusion_result")
        print "Fusion of "+str(input_BU)+" and "+str(input_TD)+" is done"
        print
#        evaluation perl .pl ground input_BU 1 1
        cmd = ["perl", "eval_metrics_v3.pl", groundtruth, input_BU,argument1,argument2]
        pipe = subprocess.Popen(cmd,stdout=subprocess.PIPE)
        pipe.wait()
        cmd = ["perl", "results2texV2.pl", "evaluation_results/"+input_BU+".L1_"+argument1+"L2_"+argument2+".results.csv"]
        pipe = subprocess.Popen(cmd,stdout=subprocess.PIPE)
        pipe.wait()
        print "Evaluation of "+str(input_BU)+ " done"
        print
#        evaluation perl .pl ground extracted 1 1
#        cmd = ["perl", "eval_metrics.pl", groundtruth, "extracted","1","1"]
        cmd = ["perl", "eval_metrics_v3.pl", groundtruth, "extracted",argument1,argument2]
        pipe = subprocess.Popen(cmd,stdout=subprocess.PIPE)
        pipe.wait()
        cmd = ["perl", "results2texV2.pl", "evaluation_results/extracted.L1_"+argument1+"L2_"+argument2+".results.csv"]
        pipe = subprocess.Popen(cmd,stdout=subprocess.PIPE)
        pipe.wait()
        print "Evaluation of the extracted fragments is done"
        
        print
#        evaluation perl .pl ground fusion 1 1
        cmd = ["perl", "eval_metrics_v3.pl", groundtruth, "fusion_result",argument1,argument2]
        pipe = subprocess.Popen(cmd,stdout=subprocess.PIPE)
        pipe.wait()
        cmd = ["perl", "results2texV2.pl", "evaluation_results/fusion_result.L1_"+argument1+"L2_"+argument2+".results.csv"]
        pipe = subprocess.Popen(cmd,stdout=subprocess.PIPE)
        pipe.wait()
        print "Evaluation of the fused fragments is done"
        


        
        
        
    
