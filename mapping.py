def mapping(extracted_file,mapped_output_file):
    
    
#    only simulation to provide a systemrun is implemented
    f_out = open(mapped_output_file,"w")
    f_out.write(simulation())
    f_out.close()




def simulation():
    return "<FROMCITY>,<ATTRIBUTE_FLIGHT>-<DATESPEC_RELATIVE>-<CITYNAME>-TO-<CITYNAME>"