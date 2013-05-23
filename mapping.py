def mapping(extracted_file,mapped_output_file):
#    write_string = ""
#    f_in = open("extracted-manualMapping1","r")
#    for line in f_in:
#        write_string += line
#    f_in.close()
#    f_out = open(mapped_output_file,"w")
##    f_out.write(simulation())
#    f_out.write(write_string)
#
#    f_out.close()
    
#    only simulation to provide a systemrun is implemented
    f_out = open(mapped_output_file,"w")
    f_out.write(simulation())
    f_out.close()




def simulation():
    write_string = ""
    f_in = open("extracted-manualMapping1","r")
    for line in f_in:
        write_string += line
    f_in.close()
    return write_string
#    return "<FROMCITY>,<ATTRIBUTE_FLIGHT>-<DATESPEC_RELATIVE>-<CITYNAME>-TO-<CITYNAME>"