#!/usr/bin/perl

## New: May 13
## Auto weight under- and over-induction

##----------------------------------------------------------------------------------------##
## Functionality:                                                                         ##
## Computes the evaluation metrics:                                                       ##
##  (i) exact-induction ratio                                                             ##
##  (ii) under-induction ratio                                                            ##
##  (iii) over-induction ratio                                                            ##
##  (iv) precision                                                                        ##
##  (v) recall                                                                            ##
## The above measurements are computed for each rule in groundtruth,                      ##
## as well as, for all rules.                                                             ##
## For further details regarding the above metrics consult the                            ##
## "grammar_evaluation.pdf".                                                              ##
##                                                                                        ##
## Input:                                                                                 ##
## 1. The name of file that correspond to groundtruth.                                    ##
##    This file should be "groundtruth/" directory.                                       ##         
## 2. The name of file that correspond to system output.                                  ##
##    This file should be "system/" directory.                                            ##
## 3. Value of lambda1 (weight of under-induction)                                        ##
## 4. Value of lambda2 (weight of over-induction)                                         ##
##                                                                                        ##
## Notes:                                                                                 ##
## - Both of the obove files given by 1st and 2nd argument should be in .csv format.      ##
## However, do not add the ".csv" extension during the passing of arguments.              ##
## - Lambdas given by 3rd and 4th argument should sum to 1, e.g., 0.5+0.5=1.              ## 
##                                                                                        ##
## Output:                                                                                ##
## A directory is created named "evaluation_results/",                                    ##
## including a .csv file named according to the name of                                   ##
## file that correspond to system output (2nd input argument)                             ##
## extended by the values of L1 and L2 and                                                ##
## ending in ".results.csv".                                                              ##
## This file includes the scores for the aforementioned evaluation metrics.               ##
##                                                                                        ##
## How to run                                                                             ##
## Example: perl evaluate.pl file_ground file_system 0.5 0.5                              ##
##                                                                                        ##
## Contact:                                                                               ##
## Elias Iosif iosife@telecom.tuc.gr                                                      ##
##----------------------------------------------------------------------------------------##

## Input args
##-----------
#------------
$ground_file = $ARGV[0]; chomp($ground_file);
$ground_system = $ARGV[1]; chomp($ground_system);
## Weights used for the computation of precision and recall
##---------------------------------------------------------
$lambda1 = $ARGV[2]; chomp($lambda1); ## Weight assigned to under-induction
$lambda2 = $ARGV[3]; chomp($lambda2); ## Weight assigned to over-induction


## Directories of groundtruth and indcuced rules
##------------------------------------------------
#$d_ground = "evaluation/groundtruth/"; ## Root directory for groundtruth
#$d_system = "evaluation/induced_rules/"; ## Root directory for induced rules

## Create directory for evaluation results
##----------------------------------------
$d_res = "evaluation_results/"; if (!(-d $d_res)) { system ("mkdir $d_res"); }

## Initalize evaluation metrics to be computed all induced rules
##--------------------------------------------------------------
$total_num_exact = 0; ## Total num. of exact matches
$total_num_under = 0; ## Total num. of partial matches: under-induction case
$total_num_over = 0; ## Total num. of partial matches: over-induction case
$total_num_under = 0; ## Total num. of partial matches: under-induction case: for auto weight
$total_num_over = 0; ## Total num. of partial matches: over-induction case: for auto weight
$total_num_none = 0; ## Total num. of members of induced rules not matched
$total_num_in_groundtruth_not_matched = 0; ## Total num. of members of groundtruth rules not matched
$total_exact_induction_ratio = 0; ## Ratio of exact-induction
$total_under_induction_ratio = 0; ## Ratio of under-induction
$total_over_induction_ratio = 0; ## Ratio of over-induction
$total_rule_precision = 0; ## Precision
$total_rule_recall = 0; ## Recall

if (scalar(@ARGV) == 0) ## No arguments given
 {
   die ("You must give the name of the file (.csv) corresponding to groundtruth and  induced rules.\n");
 }
else
 {
  $groundtruth_path = "";
  $induced_path = "";
  #$groundtruth_path = $d_ground.$ground_file;
  #$induced_path =  $d_system.$ground_system;
  $groundtruth_path = $ground_file;
  $induced_path =  $ground_system;

  if ( (!(-e $groundtruth_path)) ||  (!(-e $induced_path)) ) ## Check if the file(s) of groundtruth and induced rules exist
   {
    die ("Can not find file(s) of  groundtruth and/or induced rules: $groundtruth_path or $induced_path\n");
   }
 }


## Create file for reporting evaluation results
##---------------------------------------------
$results_file = $d_res.$ground_system.".L1_".$lambda1."L2_".$lambda1.".results.csv";
open (RES,">$results_file") || die ("Can not write $results_file \n");
print RES ("Rule",",","ExactInductionRatio",",","UnderInductionRatio",",","OverInductionRatio",",","Precision",",","Recall","\n");

## Load groundtruth
##-----------------
%groundtruth_rules2members = ();
%groundtruth_rules2members_num = ();
@members_lst = ();
$members_str = ();
@groundtruth_rules_lst_sort = ();
$rule_label = "";
$num_of_members = 0;
open (G,"$groundtruth_path") || die ("Can not open $groundtruth_path \n");
$rdln = "";
$rdln = <G>;
while ($rdln ne "")
 {
  chomp($rdln);
  $rdln =~ s/\-/ /g; ## New: May 2013
  @members_lst = split(/,/,$rdln); ## Rule label + members
  $rule_label = shift(@members_lst); ## Extract 1st element that is the rule label
  $members_str = join(",",@members_lst);
  $num_of_members = $#members_lst + 1;
  if ($num_of_members < 1)
   {
      die ("Rule $rule_label appears to be empty. Check this rule. \n");
      #print ("Rule $rule_label appears to be empty. Check this rule. \n");
   }
  $groundtruth_rules2members{$rule_label} = $members_str;
  $groundtruth_rules2members_num{$rule_label} = $num_of_members; 
  $rdln = <G>;
 }
close (G);
@groundtruth_rules_lst_sort = sort(keys(%groundtruth_rules2members));

## Load rules induced by the system
##---------------------------------
%induced_rules2members = ();
%induced_rules2members_num = ();
@members_lst = ();
$members_str = ();
$rule_label = "";
$num_of_members = 0;
open (I,"$induced_path") || die ("Can not open $groundtruth_path \n");
$rdln = "";
$rdln = <I>;
while ($rdln ne "")
 {
   $rdln =~ s/\-/ /g; ## New: May 2013
   chomp($rdln);
   @members_lst = split(/,/,$rdln); ## Rule label + members
   $rule_label = shift(@members_lst); ## Extract 1st element that is the rule label
   $members_str = join(",",@members_lst);
   $num_of_members = $#members_lst + 1;   
   $induced_rules2members{$rule_label} = $members_str;
   $induced_rules2members_num{$rule_label} = $num_of_members;
   $rdln = <I>;
 }
close (I);

## Compute evaluation metrics
##---------------------------
$g_counter = 0;

foreach $grule (@groundtruth_rules_lst_sort) ## For each groundtruth rule: start
 {
   $g_members_str = "";
   @g_members_lst = ();
   %g_members_hsh = ();
   %g_checked = ();
   %g_exact_checked = ();
   %g_under_checked = ();
   %g_over_checked = ();
   $i_members_str = "";
   @i_members_lst = ();
   %i_members_hsh = ();
   %i_checked = ();
   %i_exact_checked = ();
   %i_under_checked = ();
   %i_over_checked = ();

   $g_counter ++;

   ## Current groundtruth rule
   ##-------------------------  
   $g_members_str = $groundtruth_rules2members{$grule};
   @g_members_lst = split(/,/,$g_members_str);
   foreach (@g_members_lst) { $g_members_hsh{$_}=1; }
   
   ## Respective induced rule
   ##-------------------------
   $i_members_str = $induced_rules2members{$grule};
   @i_members_lst = split(/,/,$i_members_str);
   foreach (@i_members_lst) { $i_members_hsh{$_}=1; }

   foreach $cur_g_member (@g_members_lst) ## For each member of groundtruth rule: start
    {
      $length_g_member = 0;
      @g_lst = split(/\s+/,$cur_g_member);
      $length_g_member = $#g_lst + 1; ## Length of current member counted as number of  tokens
      
      ## Compare with members of respective induced rule
      ##------------------------------------------------
      if (defined($i_members_hsh{$cur_g_member})) ## Exact match
       {
         if ( (!(defined($g_exact_checked{$cur_g_member}))) && (!(defined($i_exact_checked{$cur_g_member}))) )
          {
           $g_checked{$cur_g_member} = 1;
           $g_exact_checked{$cur_g_member} = 1;
           $i_checked{$cur_g_member} = 1;
           $i_exact_checked{$cur_g_member} = 1;
          }
       }
      elsif ( (!(defined($i_members_hsh{$cur_g_member}))) && ($length_g_member > 1)) ## No exact match. So, check for under-induction or over-induction
       {
          %reference = ();
          map { $reference{$_} = 1 } @g_lst;

          ## Check for each member respective induced rule
          ##----------------------------------------------
          $under_induction_flag = 0;
          $over_induction_flag = 0;
          foreach $cur_i_member (@i_members_lst) ## For each member of induced  rule: start
           {
              $length_i_member = 0;
              @i_lst = split(/\s+/,$cur_i_member);
              @overlap_lst = ();
              @overlap_lst = grep { $reference{$_} } @i_lst;
              $overlap_score = 0;
              $overlap_score = $#overlap_lst + 1;
              $length_i_member = 0;
              $length_i_member = $#i_lst + 1;

              if ( ($length_i_member <= $length_g_member) && ($overlap_score > 0) )
               {
                if ( (!(defined($g_exact_checked{$cur_g_member}))) && (!(defined($i_exact_checked{$cur_g_member}))) )
                 {
                if ( (!(defined($g_under_checked{$cur_g_member}))) && (!(defined($i_under_checked{$cur_g_member}))) )
                 {
                   $g_under_checked{$cur_g_member} = 1;
                   #$i_under_checked{$cur_i_member} = 1;
                   $i_under_checked{$cur_i_member} = ($overlap_score / $length_g_member);
                   #print (@overlap_lst,"\n");
                   #print  ("Ground: ",$cur_g_member,"\n");
                   #print  ("Induced: ",$cur_i_member,"\n");
                 }
                 }   
               }
              if ( ($length_i_member > $length_g_member) && ($overlap_score > 0) )
               {
                 if ( (!(defined($g_exact_checked{$cur_g_member}))) && (!(defined($i_exact_checked{$cur_g_member}))) )
                 {
                 if ( (!(defined($g_over_checked{$cur_g_member}))) && (!(defined($i_over_checked{$cur_g_member}))) )
                 {
                   $g_over_checked{$cur_g_member} = 1;
                   #$i_over_checked{$cur_i_member} = 1;
                   $i_over_checked{$cur_i_member} = ($overlap_score / $length_i_member);;
                 }
                 }
               }
           } ## For each member of induced  rule: start
        
       } ## Not exact match
    }  ## For each member of groundtruth rule: end

   ## Evaluation metrics for current rule
   ##------------------------------------
   $num_exact = 0;
   $num_under = 0;
   $num_over = 0;
   $num_under2 = 0;
   $num_over2 = 0;
   $num_none = 0;
   $num_in_groundtruth_not_matched = 0;
   $exact_induction_ratio = 0;
   $under_induction_ratio = 0;
   $over_induction_ratio = 0;
   $current_rule_precision = 0;
   $current_rule_recall = 0;
   
   @num_exact_lst = keys(%i_exact_checked);
   $num_exact = $#num_exact_lst + 1;
   @num_under_lst = keys(%i_under_checked);
   $num_under = $#num_under_lst + 1;
   foreach (keys(%i_under_checked)) { $num_under2 += $i_under_checked{$_}; }
   @num_over_lst = keys(%i_over_checked);
   $num_over = $#num_over_lst + 1;
   foreach (keys(%i_over_checked)) { $num_over2 += $i_over_checked{$_}; }


   foreach $cur_i_mem (keys(%i_members_hsh))
    {
      if ( (!(defined($i_exact_checked{$cur_i_mem}))) && (!(defined($i_under_checked{$cur_i_mem}))) && (!(defined($i_over_checked{$cur_i_mem}))) )
       {
         $num_none ++;
       }
    }
   foreach $cur_g_mem (keys(%g_members_hsh))
    {
      if ( (!(defined($g_exact_checked{$cur_g_mem}))) && (!(defined($g_under_checked{$cur_g_mem}))) && (!(defined($g_over_checked{$cur_g_mem}))) )
       {
         $num_in_groundtruth_not_matched ++;
       }
    }

   print ("$grule\n");
   print ("$num_exact $num_under $num_over $num_none\n");


   if (($num_exact==0) && ($num_under==0) && ($num_over==0))
    {
      $num_exact = $num_under = $num_over = 0;
    }
   else
    {
   $exact_induction_ratio = $num_exact / ($num_exact + $num_under + $num_over);
   $under_induction_ratio = $num_under / ($num_exact + $num_under + $num_over);
   $over_induction_ratio = $num_over / ($num_exact + $num_under + $num_over);
    }

   if (($num_exact==0) && ($num_under==0) && ($num_over==0) && ($num_none==0))
    {
      $current_rule_precision = 0;
    }
   else
    {  
   #$current_rule_precision = ($num_exact+($lambda1*$num_under)+($lambda2*$num_over)) / ($num_exact+$num_under+$num_over+$num_none);
   $current_rule_precision = ($num_exact+($lambda1*$num_under2)+($lambda2*$num_over2)) / ($num_exact+$num_under+$num_over+$num_none);
    }

   
   #$current_rule_recall = ($num_exact+($lambda1*$num_under)+($lambda2*$num_over)) / ($num_exact+$num_under+$num_over+$num_in_groundtruth_not_matched);
   $current_rule_recall = ($num_exact+($lambda1*$num_under2)+($lambda2*$num_over2)) / ($num_exact+$num_under+$num_over+$num_in_groundtruth_not_matched);

   ## Report metrics using 3-digit precision
   ##---------------------------------------
   $exact_induction_ratio = sprintf("%.3f", $exact_induction_ratio);
   $under_induction_ratio = sprintf("%.3f", $under_induction_ratio);
   $over_induction_ratio = sprintf("%.3f", $over_induction_ratio);
   $current_rule_precision = sprintf("%.3f", $current_rule_precision);
   $current_rule_recall = sprintf("%.3f", $current_rule_recall);

   print ($grule,",",$exact_induction_ratio,",",$under_induction_ratio,",",$over_induction_ratio,",",$current_rule_precision,",",$current_rule_recall,"\n");
   print RES ($grule,",",$exact_induction_ratio,",",$under_induction_ratio,",",$over_induction_ratio,",",$current_rule_precision,",",$current_rule_recall,"\n");

   ## Update summations for the computation of overall metrics
   ##---------------------------------------------------------
   $total_num_exact += $num_exact;
   $total_num_under += $num_under;
   $total_num_over += $num_over;
   $total_num_under2 += $num_under2;
   $total_num_over2 += $num_over2;
   $total_num_none += $num_none;
   $total_num_in_groundtruth_not_matched += $num_in_groundtruth_not_matched;
 } ## For each groundtruth rule: end


## Compute overall metrics
##------------------------
$total_exact_induction_ratio = $total_num_exact / ($total_num_exact + $total_num_under + $total_num_over); 
$total_under_induction_ratio = $total_num_under / ($total_num_exact + $total_num_under + $total_num_over);
$total_over_induction_ratio = $total_num_over/ ($total_num_exact + $total_num_under + $total_num_over);


$pr_enum = 0; ## Enumerator of precision
$pr_denom = 0; ## Denominator of precision
#$pr_enum = $total_num_exact + ($lambda1*$total_num_under) + ($lambda2*$total_num_over);
$pr_enum = $total_num_exact + ($lambda1*$total_num_under2) + ($lambda2*$total_num_over2);
$pr_denom = $total_num_exact + $total_num_under + $total_num_over + $total_num_none;
$total_rule_precision = $pr_enum / $pr_denom;

$rc_enum = 0; ## Enumerator of recall
$rc_denom = 0; ## Denominator of recall
#$rc_enum = $total_num_exact + ($lambda1*$total_num_under) + ($lambda2*$total_num_over);
$rc_enum = $total_num_exact + ($lambda1*$total_num_under2) + ($lambda2*$total_num_over2);
$rc_denom = $total_num_exact + $total_num_under + $total_num_over + $total_num_in_groundtruth_not_matched;
$total_rule_recall = $rc_enum / $rc_denom;

## Report metrics using 3-digit precision
##---------------------------------------
$total_exact_induction_ratio = sprintf("%.3f", $total_exact_induction_ratio);
$total_under_induction_ratio = sprintf("%.3f", $total_under_induction_ratio);
$total_over_induction_ratio = sprintf("%.3f", $total_over_induction_ratio);
$total_rule_precision = sprintf("%.3f", $total_rule_precision);
$total_rule_recall = sprintf("%.3f", $total_rule_recall);

print ("ALL_RULES",",",$total_exact_induction_ratio,",",$total_under_induction_ratio,",",$total_over_induction_ratio,",");
print ($total_rule_precision,",",$total_rule_recall,"\n");
print RES ("ALL_RULES",",",$total_exact_induction_ratio,",",$total_under_induction_ratio,",",$total_over_induction_ratio,",");
print RES ($total_rule_precision,",",$total_rule_recall,"\n");

close (RES);
