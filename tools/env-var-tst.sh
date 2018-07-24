
showmd () {
    echo "File: $1"
    echo '````'
    head -n $2 "$1"
    echo "...."
    tail -n $2 "$1"
    echo '````'
}

env_var_test_script=/tmp/env-var-run-test.sh
echo "Preparing $env_var_test_script"
printf "set -e\n" > $env_var_test_script
for i in $(seq 100 1000) ; do
    printf "export test_$i=\"" >> $env_var_test_script
    for j in $(seq 1 $i) ; do
        printf "S" >> $env_var_test_script
    done
    printf "\"" >> $env_var_test_script
    printf "\nprintf \"\${test_$i}\" > /tmp/evt-res-$i.txt" >> $env_var_test_script
    printf "\nexport res1_$i=\"\$(wc -c /tmp/evt-res-$i.txt)\"" >> $env_var_test_script
    printf "\nexport res2_$i=\"\$(printf \"\${test_$i}\" | wc -c)\"" >> $env_var_test_script
    printf "\nexport res3_$i=\"\$(env | wc -c)\"" >> $env_var_test_script
    printf "\necho \"Done with $i: \$res1_$i - \$res2_$i chars ... env \$res3_$i\"\n" >> $env_var_test_script
done

showmd "$env_var_test_script" 5

printf "\n\nRunning $env_var_test_script with dash\n\n"
dash $env_var_test_script > /tmp/dash-env-var-tst.out 2> /tmp/dash-env-var-tst.err

echo ""
showmd /tmp/dash-env-var-tst.out 5
echo ""
showmd /tmp/dash-env-var-tst.err 5

# printf "\n\nRunning $env_var_test_script with bash\n\n"
# bash $env_var_test_script | tail
# 
# printf "\n\nRunning $env_var_test_script with sh\n\n"
# sh $env_var_test_script | tail

