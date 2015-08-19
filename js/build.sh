SRC_DIR=src
BUILD_DIR=bin
mkdir -p ${BUILD_DIR}

cat ${SRC_DIR}/grammar.pegjs > ${BUILD_DIR}/_grammar.pegjs
cat ${SRC_DIR}/_common.pegjs >> ${BUILD_DIR}/_grammar.pegjs
pegjs ${BUILD_DIR}/_grammar.pegjs ${BUILD_DIR}/grammar.js

cat ${SRC_DIR}/indent.pegjs > ${BUILD_DIR}/_indent.pegjs
cat ${SRC_DIR}/_common.pegjs >> ${BUILD_DIR}/_indent.pegjs
pegjs ${BUILD_DIR}/_indent.pegjs ${BUILD_DIR}/indent.js

cp ${SRC_DIR}/test_script.js ${BUILD_DIR}/test_script.js
