./scenario1/state_space.exe
chmod +r states.csv trans.csv
cp scenario1/trans2timeshift.csv t2ts.csv
cp scenario1/agent_ctrls.csv ac.csv
./semi_transform.exe states.csv trans.csv t2ts.csv ac.csv Scenario1 scenario1_policy_template 2 true
rm t2ts.csv
rm ac.csv
cp scenario1/dest_patterns.csv dp.csv
./find_dest_states.exe states.csv dp.csv destination_states
rm dp.csv