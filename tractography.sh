#!/bin/sh

#tractography.sh
#
#
#  Created by Mark Bowren on 4/26/20.
#
#Place white matter clusters into seed directory

cd /Users/mbowren/Documents/uiowa/core/tractography/seeds

cp /Users/mbowren/Documents/uiowa/core/lsm/lesymap/running/sem_z/g/hi/sccan/overlap_3/clusters/bin/*.nii.gz .

#Copy over to psychiatry for futher processing

scp *.nii.gz mbowren@itf03.psychiatry.uiowa.edu:/Shared/boeslab/Users/Bowren/g/tractography/seeds

#Resample your MNI152 space file into MNI2009b asymetric, make binary, uncompress (for matlab); do this in psychiatry itf06

ssh -Y mbowren@itf03.psychiatry.uiowa.edu

inDir=/Shared/boeslab/brussj/Tranel_lesionData
atlasDir=/Shared/boeslab/parcellationAtlases/MNI152_2009_atlases/2009b_asymmetric
xfmDir=${atlasDir}/0.5mm/_reg-MNI152_1mm
ANTSPATH=/Shared/pinc/sharedopt/apps/ants/Linux/x86_64/2.2.0-old/bin
outDir=/Shared/boeslab/Users/Bowren/g/tractography/seeds/2009a

#INPUT/OUTPUT will be whatever you have for input files and what you want output name to be (use .nii.gz extension)

cd /Shared/boeslab/Users/Bowren/g/tractography/seeds

for seed in *.nii.gz
do
${ANTSPATH}/antsApplyTransforms -d 3 \
-i ${seed} \
-r ${atlasDir}/0.5mm/mni_icbm152_t1_tal_nlin_asym_09b_hires.nii \
-o ${outDir}/xfm_${seed} \
-t [${xfmDir}/mni_icbm152_t1_tal_nlin_asym_09b_hires_reg-MNI152_T1_1mm_0GenericAffine.mat,1] \
-t ${xfmDir}/mni_icbm152_t1_tal_nlin_asym_09b_hires_reg-MNI152_T1_1mm_1InverseWarp.nii.gz \
-n NearestNeighbor
fslmaths ${outDir}/xfm_${seed} -bin ${outDir}/xfm_${seed} -odt char
gunzip ${outDir}/xfm_${seed}
done

#Use matlab to run tractography analyses through Lead Mapper GUI

matlab

#Copy back to neurology

exit

cd /Shared/boeslab/Users/Bowren/core/tractography/lead_dbs/results

#Rename files

mv xfm_bin_cluster01_struc_seed.nii struc_seed_cluster01.nii.gz
mv xfm_bin_cluster02_struc_seed.nii struc_seed_cluster02.nii.gz
mv xfm_bin_cluster03_struc_seed.nii struc_seed_cluster03.nii.gz
mv xfm_bin_cluster04_struc_seed.nii struc_seed_cluster04.nii.gz
mv xfm_bin_cluster05_struc_seed.nii struc_seed_cluster05.nii.gz
mv xfm_bin_cluster06_struc_seed.nii struc_seed_cluster06.nii.gz
mv xfm_bin_cluster07_struc_seed.nii struc_seed_cluster07.nii.gz
mv xfm_bin_cluster08_struc_seed.nii struc_seed_cluster08.nii.gz
mv xfm_bin_cluster09_struc_seed.nii struc_seed_cluster09.nii.gz

#Combine raw tractography results into one file by adding them

cd /Users/mbowren/Documents/uiowa/core/tractography/lead_dbs/results

fslmaths struc_seed_cluster01.nii.gz -add struc_seed_cluster02.nii.gz -add struc_seed_cluster03.nii.gz -add struc_seed_cluster04.nii.gz -add struc_seed_cluster05.nii.gz -add struc_seed_cluster06.nii.gz -add struc_seed_cluster07.nii.gz -add struc_seed_cluster08.nii.gz -add struc_seed_cluster09.nii.gz sum_tracts.nii.gz
