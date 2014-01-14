#! /usr/lib/perl 

use strict;
use warnings;

# Hacks
my $threshold = 1;

# Notes
# Evaluations: modify eval_metrics, check metric (calculate both??)

# Directories
my $root_dir = "";
my $src_dir = $root_dir."source/";
chdir($src_dir); $root_dir = "../";
my $data_dir = $root_dir."data/";
my $eval_dir = "evaluation_results/";
my $res_dir = $root_dir."results/";

# Arguments - statics or variables
my $bu_gram = $ARGV[0];
my $td_gram = $ARGV[1];
my $groundtruth = $ARGV[2];
my $flag = $ARGV[3];
my $fu_gram_base = "fusion_result."; #$ARGV[1];
my $fu_gram;
my $mapBUtoTDground = $data_dir."mappings_BU_to_TD.json";
my $mapTDtoBUground = $data_dir."mappings_TD_to_BU.json";
my @metrics = ("0"); 		# 0 for Levenshtein, 1 for LCS
my @mappings = ("0"); 		# 0 for td-to-bu, 1 for reverse
my @strategies = ("1", "2", "3");	# 1=> Simple Union, 2=> Top Down Augmentation, 3=> Bottom Up Augmentation
my @thresholds = ("1"); 		# 0-1, 1 for no threshold

# BEGIN

# Take out the garbage
system("rm extracted_BU");
system("rm extracted_TD");
system("rm extracted_TD_extracted_BU.lexsem");

if (($flag == 0)||($flag == 1)) {
# Step 1. Input Grammars
# ======================
print "Step 1. Input Grammars\n";
print "======================\n";
system("cp ../data/$bu_gram ./");
system("cp ../data/$td_gram ./");
system("cp ../data/$groundtruth ./");
# 	(a) Computation
#	---------------

#	(b) Evaluation
#	--------------

# 	1. Bottom Up grammar with Golden Standard Grammar
	&evaluate($bu_gram);

# 	2. Top Down grammar with Golden Standard Grammar
	&evaluate($td_gram);

#	(c) Archive
	my $step1_path = $res_dir."1_InputGrammars/";
	system("mkdir ".$step1_path) if (! -e $step1_path);
	system("mv ./eval_".$bu_gram." ".$step1_path);
	system("mv ./eval_".$td_gram." ".$step1_path);
	system("mv ./".$bu_gram." ".$step1_path);
	system("mv ./".$td_gram." ".$step1_path);
	system("mv ./".$bu_gram."_".$groundtruth.".lexsem ".$step1_path);
	system("mv ./".$td_gram."_".$groundtruth.".lexsem ".$step1_path);
	system("mv ./".$groundtruth." ".$step1_path);
	system("mv ".$eval_dir."eval_".$bu_gram.".L1_1L2_1.results.csv ".$step1_path);
	system("mv ".$eval_dir."eval_".$td_gram.".L1_1L2_1.results.csv ".$step1_path);
	print "Data archived\n";

}
if (($flag == 0)||($flag == 2)) {
# Step 2. Union No1 of BU and TD (no concept mapping)
# ===================================================
print "Step 2. Union No1 of BU and TD (no concept mapping)\n";
print "===================================================\n";
system("cp ../data/$bu_gram ./");
system("cp ../data/$td_gram ./");
system("cp ../data/$groundtruth ./");

#	(a) Computation
#	---------------
#	1. ONLY simple union of the two grammars (merging), without any similarity calculation, mapping or rule merging (not even when they have common terms)
	system("python start.py $bu_gram $td_gram $groundtruth 1");

#	(b) Evaluation
#	--------------
#	1. Fusion grammar (from simple union) with Golden Standard Grammar
	$fu_gram = $fu_gram_base."1"; &evaluate($fu_gram);

#	(c) Archive
	my $step2_path = $res_dir."2_SimpleUnion_NoConceptMap/";
	system("mkdir ".$step2_path) if (! -e $step2_path);
	system("mv ./eval_".$fu_gram." ".$step2_path);
	system("mv ".$bu_gram." ".$step2_path);
	system("mv ".$td_gram." ".$step2_path);
	system("mv ".$fu_gram." ".$step2_path);
	system("mv ./".$fu_gram."_".$groundtruth.".lexsem ".$step2_path);
	system("mv ./".$groundtruth." ".$step2_path);
	system("mv ".$eval_dir."eval_".$fu_gram.".L1_1L2_1.results.csv ".$step2_path); 
	print "Data archived\n";
}
if (($flag == 0)||($flag == 3)) {
# Step 3. Union No2 of BU and TD (with concept mapping)
# =====================================================
print "Step 3. Union No2 of BU and TD (with concept mapping)\n";
print "=====================================================\n";
system("cp ../data/$bu_gram ./");
system("cp ../data/$td_gram ./");
system("cp ../data/$groundtruth ./");
my $step3_path = $res_dir."3_Fusions_WithConceptMap/";
system("mkdir ".$step3_path) if (! -e $step3_path);
#	(a) Computation
#	---------------
#	1. Mapping of BU grammar's concepts to the TD
	print "Mapping of BU grammar's concepts to the TD...\n";
	system("perl extract_rules.pl $bu_gram $td_gram 0 0");
#	2. Augmentation of both grammars and simple union
	print "Fusing...\n";
	system("python start.py $bu_gram $td_gram $groundtruth 1");	
	$fu_gram = $fu_gram_base."1"; system("mv ./$fu_gram ./$fu_gram.td");
	system("python start.py $bu_gram $td_gram $groundtruth 2");
	$fu_gram = $fu_gram_base."2"; system("mv ./$fu_gram ./$fu_gram.td");
	system("python start.py $bu_gram $td_gram $groundtruth 3");
	$fu_gram = $fu_gram_base."3"; system("mv ./$fu_gram ./$fu_gram.td");
	system("mv ./".$bu_gram." ".$step3_path.$bu_gram.".td.concepted");
	system("mv ./".$td_gram." ".$step3_path.$td_gram.".td.concepted");

	system("cp ../data/$bu_gram ./");
	system("cp ../data/$td_gram ./");
#	3. Mapping of TD grammar's concepts to the BU
	print "Mapping of TD grammar's concepts to the BU...\n";
	system("perl extract_rules.pl $td_gram $bu_gram 0 0");
#	4. Augmentation of both grammars and simple union
	print "Fusing...\n";
	system("python start.py $bu_gram $td_gram $groundtruth 1");	
	$fu_gram = $fu_gram_base."1"; system("mv ./$fu_gram ./$fu_gram.bu");
	system("python start.py $bu_gram $td_gram $groundtruth 2");
	$fu_gram = $fu_gram_base."2"; system("mv ./$fu_gram ./$fu_gram.bu");
	system("python start.py $bu_gram $td_gram $groundtruth 3");
	$fu_gram = $fu_gram_base."3"; system("mv ./$fu_gram ./$fu_gram.bu");
	system("mv ./".$td_gram." ".$step3_path.$td_gram.".bu.concepted");
	system("mv ./".$bu_gram." ".$step3_path.$bu_gram.".bu.concepted");

#	(b) Evaluation
#	--------------
#	1. Concept Mapping performance (from BU to TD mapping) with BU-to-TD Golden Standard Concept Mapping
	system("perl evalConceptMappings.pl concepts_".$bu_gram."_concepts_".$td_gram.".lexsem concepts_".$bu_gram."_concepts_".$td_gram.".lexsem.most $mapBUtoTDground");
#	2. Augmented TD (from BU to TD mapping) with Golden Standard Grammar
	$fu_gram = $fu_gram_base."2.td"; &evaluate($fu_gram);
#	3. Augmented BU (from BU to TD mapping) with Golden Standard Grammar
	$fu_gram = $fu_gram_base."3.td"; &evaluate($fu_gram);
#	4. Fusion Grammar (from BU to TD mapping) with Golden Standard Grammar
	$fu_gram = $fu_gram_base."1.td"; &evaluate($fu_gram);
#	5. Concept Mapping performance (from TD to BU mapping) with TD-to-BU Golden Standard Concept Mapping
	system("perl evalConceptMappings.pl concepts_".$td_gram."_concepts_".$bu_gram.".lexsem concepts_".$td_gram."_concepts_".$bu_gram.".lexsem.most $mapTDtoBUground");
#	6. Augmented TD (from TD to BU mapping) with Golden Standard Grammar
	$fu_gram = $fu_gram_base."2.bu"; &evaluate($fu_gram);
#	7. Augmented BU (from TD to BU mapping) with Golden Standard Grammar
	$fu_gram = $fu_gram_base."3.bu"; &evaluate($fu_gram);
#	8. Fusion Grammar (from TD to BU mapping) with Golden Standard Grammar
	$fu_gram = $fu_gram_base."1.bu"; &evaluate($fu_gram);

#	(c) Archive
#	--------------
	system("mkdir ".$step3_path) if (! -e $step3_path);
	system("mv ./concepts_".$bu_gram." ".$step3_path.$bu_gram.".concepts");
	system("mv ./concepts_".$bu_gram."_concepts_".$td_gram.".lexsem ".$step3_path);
	system("mv ./concepts_".$bu_gram."_concepts_".$td_gram.".lexsem.most ".$step3_path);
	system("mv ./concepts_".$bu_gram."_concepts_".$td_gram.".lexsem.results.csv ".$step3_path);
	system("mv ./concepts_".$td_gram." ".$step3_path.$td_gram.".concepts");
	system("mv ./concepts_".$td_gram."_concepts_".$bu_gram.".lexsem ".$step3_path);
	system("mv ./concepts_".$td_gram."_concepts_".$bu_gram.".lexsem.most ".$step3_path);
	system("mv ./concepts_".$td_gram."_concepts_".$bu_gram.".lexsem.results.csv ".$step3_path);
	$fu_gram = $fu_gram_base."1.td"; system("mv ".$fu_gram." ".$step3_path); system("mv ./".$fu_gram."_".$groundtruth.".lexsem ".$step3_path); system("mv eval_".$fu_gram." ".$step3_path); system("mv ".$eval_dir."eval_".$fu_gram.".L1_1L2_1.results.csv ".$step3_path); 
	$fu_gram = $fu_gram_base."2.td"; system("mv ".$fu_gram." ".$step3_path); system("mv ./".$fu_gram."_".$groundtruth.".lexsem ".$step3_path); system("mv eval_".$fu_gram." ".$step3_path); system("mv ".$eval_dir."eval_".$fu_gram.".L1_1L2_1.results.csv ".$step3_path); 
	$fu_gram = $fu_gram_base."3.td"; system("mv ".$fu_gram." ".$step3_path); system("mv ./".$fu_gram."_".$groundtruth.".lexsem ".$step3_path); system("mv eval_".$fu_gram." ".$step3_path); system("mv ".$eval_dir."eval_".$fu_gram.".L1_1L2_1.results.csv ".$step3_path); 
	$fu_gram = $fu_gram_base."1.bu"; system("mv ".$fu_gram." ".$step3_path); system("mv ./".$fu_gram."_".$groundtruth.".lexsem ".$step3_path); system("mv eval_".$fu_gram." ".$step3_path); system("mv ".$eval_dir."eval_".$fu_gram.".L1_1L2_1.results.csv ".$step3_path); 
	$fu_gram = $fu_gram_base."2.bu"; system("mv ".$fu_gram." ".$step3_path); system("mv ./".$fu_gram."_".$groundtruth.".lexsem ".$step3_path); system("mv eval_".$fu_gram." ".$step3_path); system("mv ".$eval_dir."eval_".$fu_gram.".L1_1L2_1.results.csv ".$step3_path); 
	$fu_gram = $fu_gram_base."3.bu"; system("mv ".$fu_gram." ".$step3_path); system("mv ./".$fu_gram."_".$groundtruth.".lexsem ".$step3_path); system("mv eval_".$fu_gram." ".$step3_path); system("mv ".$eval_dir."eval_".$fu_gram.".L1_1L2_1.results.csv ".$step3_path); 
	system("mv ./".$groundtruth." ".$step3_path);
	print "Data archived\n";
}

if (($flag == 0)||($flag == 4)) {
# Step 4. Union No3 of BU and TD (with concept & rule mapping)
# ============================================================
print "Step 4. Union No3 of BU and TD (with concept & rule mapping)\n";
print "============================================================\n";
system("cp ../data/$bu_gram ./");
system("cp ../data/$td_gram ./");
system("cp ../data/$groundtruth ./");
my $step4_path = $res_dir."4_Fusions_WithRuleMap/";
system("mkdir ".$step4_path) if (! -e $step4_path);
#	(a) Computation
#	---------------
#	1. Mapping of BU grammar's concepts to the TD
	print "\nMapping of BU grammar's concepts to the TD...\n";
	system("perl extract_rules.pl $bu_gram $td_gram 0 0");
#	2. Mapping of BU grammar's rules to the TD
	print "\nMapping of BU grammar's rules to the TD...\n";
	system("perl extract_rules.pl $bu_gram $td_gram 0 1");
#	3. Fusion
	print "\nFusing...\n";
	system("python start.py $bu_gram $td_gram $groundtruth 2");
	$fu_gram = $fu_gram_base."2"; system("mv ./$fu_gram ./$fu_gram.td");
	system("mv ./".$bu_gram." ".$step4_path.$bu_gram.".td.ruled");
	system("mv ./".$td_gram." ".$step4_path.$td_gram.".td.ruled");

	system("cp ../data/$bu_gram ./");
	system("cp ../data/$td_gram ./");
#	4. Mapping of TD grammar's concepts to the BU
	print "\nMapping of TD grammar's concepts to the BU...\n";
	system("perl extract_rules.pl $td_gram $bu_gram 0 0");
#	5. Mapping of TD grammar's rules to the BU
	print "\nMapping of TD grammar's rules to the BU...\n";
	system("perl extract_rules.pl $td_gram $bu_gram 0 1");
#	6. Fusion
	print "\nFusing...\n";	
	system("python start.py $bu_gram $td_gram $groundtruth 3");
	$fu_gram = $fu_gram_base."3"; system("mv ./$fu_gram ./$fu_gram.bu");	
	system("mv ./".$td_gram." ".$step4_path.$td_gram.".bu.ruled");
	system("mv ./".$bu_gram." ".$step4_path.$bu_gram.".bu.ruled");

#	(b) Evaluation
#	--------------
	print "\n========\nEvaluating\n========\n";
#	1. Concept Mapping performance (from BU to TD mapping) with BU-to-TD Golden Standard Concept Mapping
	print "\nConcept Mapping...\n";
	system("perl evalConceptMappings.pl concepts_".$bu_gram."_concepts_".$td_gram.".lexsem concepts_".$bu_gram."_concepts_".$td_gram.".lexsem.most $mapBUtoTDground");
#	2. Rule Mapping performance (from BU to TD mapping) with BU-to-TD Golden Standard Rule Mapping
	print "\nRule Mapping...\n";
	system("perl evalConceptMappings.pl rules_".$bu_gram."_rules_".$td_gram.".lexsem rules_".$bu_gram."_rules_".$td_gram.".lexsem.most $mapBUtoTDground");
	print "\nFusions...\n";
#	3. Augmented TD (from BU to TD mapping) with Golden Standard Grammar
	print "Augmented TD (from BU to TD mapping)\n";
	$fu_gram = $fu_gram_base."2.td"; &evaluate($fu_gram);

#	6. Concept Mapping performance (from TD to BU mapping) with TD-to-BU Golden Standard Concept Mapping
	print "\nConcept Mapping...\n";
	system("perl evalConceptMappings.pl concepts_".$td_gram."_concepts_".$bu_gram.".lexsem concepts_".$td_gram."_concepts_".$bu_gram.".lexsem.most $mapTDtoBUground");
#	7. Rule Mapping performance (from TD to BU mapping) with TD-to-BU Golden Standard Rule Mapping
	print "\nRule Mapping...\n";
	system("perl evalConceptMappings.pl rules_".$td_gram."_rules_".$bu_gram.".lexsem rules_".$td_gram."_rules_".$bu_gram.".lexsem.most $mapTDtoBUground");
	print "\nFusions...\n";
#	9. Augmented BU (from TD to BU mapping) with Golden Standard Grammar
	print "Augmented BU (from TD to BU mapping)\n";
	$fu_gram = $fu_gram_base."3.bu"; &evaluate($fu_gram);

#	(c) Archive
#	--------------
	system("mkdir ".$step4_path) if (! -e $step4_path);
	system("mv ./concepts_".$bu_gram." ".$step4_path.$bu_gram.".concepts");
	system("mv ./concepts_".$bu_gram."_concepts_".$td_gram.".lexsem ".$step4_path);
	system("mv ./concepts_".$bu_gram."_concepts_".$td_gram.".lexsem.most ".$step4_path);
	system("mv ./concepts_".$bu_gram."_concepts_".$td_gram.".lexsem.results.csv ".$step4_path);
	system("mv ./concepts_".$td_gram." ".$step4_path.$td_gram.".concepts");
	system("mv ./concepts_".$td_gram."_concepts_".$bu_gram.".lexsem ".$step4_path);
	system("mv ./concepts_".$td_gram."_concepts_".$bu_gram.".lexsem.most ".$step4_path);
	system("mv ./concepts_".$td_gram."_concepts_".$bu_gram.".lexsem.results.csv ".$step4_path);
	system("mv ./rules_".$bu_gram." ".$step4_path.$bu_gram.".rules");
	system("mv ./rules_".$bu_gram."_rules_".$td_gram.".lexsem ".$step4_path);
	system("mv ./rules_".$bu_gram."_rules_".$td_gram.".lexsem.most ".$step4_path);
	system("mv ./rules_".$bu_gram."_rules_".$td_gram.".lexsem.results.csv ".$step4_path);
	system("mv ./rules_".$td_gram." ".$step4_path.$td_gram.".rules");
	system("mv ./rules_".$td_gram."_rules_".$bu_gram.".lexsem ".$step4_path);
	system("mv ./rules_".$td_gram."_rules_".$bu_gram.".lexsem.most ".$step4_path);
	system("mv ./rules_".$td_gram."_rules_".$bu_gram.".lexsem.results.csv ".$step4_path);
	$fu_gram = $fu_gram_base."2.td"; system("mv ".$fu_gram." ".$step4_path); system("mv ./".$fu_gram."_".$groundtruth.".lexsem ".$step4_path); system("mv eval_".$fu_gram." ".$step4_path); system("mv ".$eval_dir."eval_".$fu_gram.".L1_1L2_1.results.csv ".$step4_path);
	$fu_gram = $fu_gram_base."3.bu"; system("mv ".$fu_gram." ".$step4_path); system("mv ./".$fu_gram."_".$groundtruth.".lexsem ".$step4_path); system("mv eval_".$fu_gram." ".$step4_path); system("mv ".$eval_dir."eval_".$fu_gram.".L1_1L2_1.results.csv ".$step4_path); 
	system("mv ./".$groundtruth." ".$step4_path);
	print "Data archived\n";
}

chdir("./results/");
system ("perl createReport.pl");

exit;

# ===========
# SUBROUTINES
# ===========

sub evaluate {
	my ($file, $f) = @_;
	system("perl map_v2.pl ".$file." ".$groundtruth." lexsem 0"); # map
	system("perl eval_export.pl ".$file." ".$file."_".$groundtruth.".lexsem $threshold"); # rename
	system("perl eval_metrics_v3.pl ".$groundtruth." eval_".$file." 1 1"); # evaluate
}
exit;
