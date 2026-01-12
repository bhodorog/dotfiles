if [[ ! -v BHG_DEBUG ]]; then
    export BHG_DEBUG=''
    [[ ${BHG_DEBUG} == 'v'* ]] &&  echo 'Debug enable, check ~/.bhg_debug.bash'
fi
