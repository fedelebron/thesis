#!/bin/zsh

IDENTIFIER=$RANDOM
[[ $ARGC > 0 ]] && IDENTIFIER=$argv[1]
MODEL_ID="test_${IDENTIFIER}"
MODEL_NAME="${MODEL_ID}.zpl"

GENERATE_DIR="$HOME/tesis/utils/"
GENERATE_CMD="./generate_testcase --professors=4 \
                                  --courses=2\
                                  --classes=1\
                                  --availability_probability=1\
                                  --start_dates=1\
                                  --schedules=2\
                                  --max_roles=1\
                                  --roles=1\
                                  --weeks=2\
                                  --week_days=4\
                                  --random_seed=1234 > $MODEL_NAME"
CONVERT_CMD="./zimpl.bin $MODEL_NAME"
FPORTA_DIR="$HOME/tesis/enumerate/"
FPORTA_CMD="./fporta $MODEL_ID.lp $MODEL_ID.txt"
MODEL_DIR="$FPORTA_DIR/$MODEL_ID"

print "Creating $MODEL_NAME..."
mkdir $MODEL_ID
pushd $GENERATE_DIR
eval $GENERATE_CMD
print "Converting to LP format..."
eval $CONVERT_CMD
mv $MODEL_ID.lp $MODEL_ID.zpl $MODEL_ID.tbl $MODEL_DIR
popd
cp fporta xporta enumerate $MODEL_ID
print "Saving config to ${MODEL_ID}/config"
pushd $MODEL_ID
print $GENERATE_CMD >> config
print "Running PORTA..."
eval $FPORTA_CMD
print "Inequalities written to $MODEL_ID.txt"
rm fporta xporta enumerate
popd $MODEL_ID

