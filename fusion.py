#This file creates a fusion of to sets of grammar fragments and returns a combination of it to a given file
import subprocess
import simple_union
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
#Chose here the fusion strategy!
#first input is augmented - do not change input here, but in the start.py
#    cmd = ["perl", "augment_fusion.pl", input1, input2,output]
#    pipe = subprocess.Popen(cmd,stdout=subprocess.PIPE)
#    pipe.wait()
    
    simple_union.fuse(input1,input2,output)
    
    
    
    
    