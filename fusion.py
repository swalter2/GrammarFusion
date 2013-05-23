#This file creates a fusion of to sets of grammar fragments and returns a combination of it to a given file
import subprocess
def fusion(input1,input2):
#    More advanced fusion have to be add here
    f_in1 = open(input1,"r")
    f_in2 = open(input2,"r")
    fusion_string = ""
    
    for line in f_in2:
        fusion_string += line
    
    
    
    f_in1.close()
    f_in2.close()
    
    return fusion_string


    
def start_fusion(input1, input2, output):
    
##if __name__ == "__main__":
##    if len(sys.argv) <4:
##        print "python fusion.py input_gram1.txt input_gram2.txt outputfile"
##        exit(1)
##    input1 = sys.argv[1]
##    input2 = sys.argv[2]
##    output = sys.argv[3]
#    
#    return_string = fusion(input1,input2)
#    f_out = open(output,"w")
#    f_out.write(return_string)
#    f_out.close()
    cmd = ["perl", "fuse.pl", input1, input2,output]
    pipe = subprocess.Popen(cmd,stdout=subprocess.PIPE)
    pipe.wait()
    
    
    