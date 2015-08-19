/*
	Executable designed for plug in to the integration/validation suite.

	Reads yamelot on stdin; emits json on stdout.
*/
package main

import (
	"encoding/json"
	"io/ioutil"
	"os"

	"github.com/dropbox/yamelot/go/yaml"
)

func main() {
	input, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	var value interface{}
	err = yaml.Unmarshal(input, &value)
	if err != nil {
		panic(err)
	}
	json.NewEncoder(os.Stdout).Encode(value)
}
