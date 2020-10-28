./demo_policy/state_space.exe
chmod +r states.csv trans.csv
cp demo_policy/trans2timeshift.csv t2ts.csv
cp demo_policy/agent_ctrls.csv ac.csv
./transform_multi_files.exe states.csv trans.csv t2ts.csv ac.csv demo demo_policy_template 1
rm t2ts.csv
rm ac.csv
cp demo_policy/dest_patterns.csv dp.csv
./find_dest_states.exe states.csv dp.csv destination_states
rm dp.csv