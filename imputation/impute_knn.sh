#PBS -l walltime=24:00:00
#PBS -l select=1:ncpus=32:mem=256gb
#PBS -N imputation

cd /rds/general/user/dba22/home/tds/Group1/tds-proj/final
module load anaconda3/personal
source activate hda

python impute_knn.py
