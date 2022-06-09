InputFolder=/vast/home/xinhuazhang/Downloads/Stack1M_Compressed/
OutputFolder=/vast/home/xinhuazhang/Workspaces/ParticleTracking/output/Tracks
MaxNumFrame=300
PixelTreshold=1000
ProbThreshold=0.2
Sigma=0.2
Tau=20
NumDir=90
NumSpeed=1
MeanSpeed=1
MinSpeed=1
MaxSpeed=1
SpeedSTD=1
MaxNumParticle=100
Threads=64

rm -r ${OutputFolder}
stack build ParticleTracking:ParticleTracking-exe
time stack exec -- ParticleTracking-exe -F ${InputFolder} -O ${OutputFolder} -p ${PixelTreshold} -P ${ProbThreshold} --sigma ${Sigma} --tau ${Tau} --numDir ${NumDir} --numSpeed ${NumSpeed} --meanSpeed ${MeanSpeed} --minSpeed ${MinSpeed} --maxSpeed ${MaxSpeed} --speedSTD ${SpeedSTD} --maxNumParticle ${MaxNumParticle} --maxNumFrame ${MaxNumFrame} +RTS -N${Threads} -RTS
