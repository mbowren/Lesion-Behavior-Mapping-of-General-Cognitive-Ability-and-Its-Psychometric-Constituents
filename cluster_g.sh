#!/bin/sh

#  cluster_g.sh
#  
#
#  Created by Mark Bowren on 4/24/20.
#

#Change to directory that has LSM of g hierarchical

cd /Users/mbowren/Documents/uiowa/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3

#Create a binary mask of the result by first making it positive, then binarizing it

fslmaths g_hi.nii.gz -mul -1 -bin bin_mul-1_g_hi.nii.gz

#Cluster the result from the last line of code

cluster -i bin_mul-1_g_hi.nii.gz -t 0.0001 -o clusters.nii.gz

#Separate out into different images based on cluster's information

fslmaths clusters.nii.gz -uthr 1 -thr 1 cluster01.nii.gz
fslmaths clusters.nii.gz -uthr 2 -thr 2 cluster02.nii.gz
fslmaths clusters.nii.gz -uthr 3 -thr 3 cluster03.nii.gz
fslmaths clusters.nii.gz -uthr 4 -thr 4 cluster04.nii.gz
fslmaths clusters.nii.gz -uthr 5 -thr 5 cluster05.nii.gz
fslmaths clusters.nii.gz -uthr 6 -thr 6 cluster06.nii.gz
fslmaths clusters.nii.gz -uthr 7 -thr 7 cluster07.nii.gz
fslmaths clusters.nii.gz -uthr 8 -thr 8 cluster08.nii.gz
fslmaths clusters.nii.gz -uthr 9 -thr 9 cluster09.nii.gz

#Move clusters into their own directory

mkdir clusters

mv *cluster0*.nii.gz clusters

cd /Users/mbowren/Documents/uiowa/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3/clusters

mkdir bin

for cluster in *.nii.gz
do
fslmaths ${cluster} -bin bin_${cluster}
done

mv bin_* bin

#Copy files into psychiatry

scp -r /fmri/studies/Bowren/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3 mbowren@itf03.psychiatry.uiowa.edu:/Shared/boeslab/Users/Bowren/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3/clusters

#SSH into adams in neurology to remove each cluster from patient masks in master directory (make sure to binarize after subtraction)

ssh mbowren@adams.neurology.uiowa.edu

cd /fmri/studies/Bowren/master/nii

mkdir sub_cluster01
mkdir sub_cluster02
mkdir sub_cluster03
mkdir sub_cluster04
mkdir sub_cluster05
mkdir sub_cluster06
mkdir sub_cluster07
mkdir sub_cluster08
mkdir sub_cluster09

for mask in *.nii.gz
do
fslmaths ${mask} -sub /fmri/studies/Bowren/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3/clusters/bin/bin_cluster01.nii.gz -bin subC1_${mask}
mv subC1_${mask} sub_cluster01
fslmaths ${mask} -sub /fmri/studies/Bowren/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3/clusters/bin/bin_cluster02.nii.gz -bin subC2_${mask}
mv subC2_${mask} sub_cluster02
fslmaths ${mask} -sub /fmri/studies/Bowren/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3/clusters/bin/bin_cluster03.nii.gz -bin subC3_${mask}
mv subC3_${mask} sub_cluster03
fslmaths ${mask} -sub /fmri/studies/Bowren/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3/clusters/bin/bin_cluster04.nii.gz -bin subC4_${mask}
mv subC4_${mask} sub_cluster04
fslmaths ${mask} -sub /fmri/studies/Bowren/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3/clusters/bin/bin_cluster05.nii.gz -bin subC5_${mask}
mv subC5_${mask} sub_cluster05
fslmaths ${mask} -sub /fmri/studies/Bowren/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3/clusters/bin/bin_cluster06.nii.gz -bin subC6_${mask}
mv subC6_${mask} sub_cluster06
fslmaths ${mask} -sub /fmri/studies/Bowren/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3/clusters/bin/bin_cluster07.nii.gz -bin subC7_${mask}
mv subC7_${mask} sub_cluster07
fslmaths ${mask} -sub /fmri/studies/Bowren/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3/clusters/bin/bin_cluster08.nii.gz -bin subC8_${mask}
mv subC8_${mask} sub_cluster08
fslmaths ${mask} -sub /fmri/studies/Bowren/core/lsm/lesymap/finished/iowa/sem_z/g/hi/sccan/overlap_3/clusters/bin/bin_cluster09.nii.gz -bin subC9_${mask}
mv subC9_${mask} sub_cluster09
done

#Move all new directories into a single g directory

mkdir g_sub_clusters

mv sub_cluster* g_sub_clusters

#See lesymap.R for how these masks were used to do cluster analyses in LESYMAP
#See tractography.R for how clusters from map of g were used to set up tractography in Lead Mapper from LEAD DBS
