/*
	Executable designed for plug in to the integration/validation suite.

	Reads yamelot on stdin; emits json on stdout.
*/
package main

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"os"

	"github.com/dropbox/yamelot/go/yaml"
)

func main() {
	bounce(os.Stdin, os.Stdout)
}

func bounce(in io.Reader, out io.Writer) {
	input, err := ioutil.ReadAll(in)
	if err != nil {
		die(err)
	}

	var value interface{}
	err = yaml.Unmarshal(input, &value)
	if err != nil {
		die(err)
	}

	value = stringifyMapKeys(value)

	err = json.NewEncoder(out).Encode(value)
	if err != nil {
		die(err)
	}
}

func die(err error) {
	fmt.Fprintf(os.Stderr, "\nFailed: %s\n", err)
	os.Exit(4)
}

func stringifyMapKeys(value interface{}) interface{} {
	switch value := value.(type) {
	case map[interface{}]interface{}:
		next := make(map[string]interface{}, len(value))
		for k, v := range value {
			next[k.(string)] = stringifyMapKeys(v)
		}
		return next
	case []interface{}:
		for i := 0; i < len(value); i++ {
			value[i] = stringifyMapKeys(value[i])
		}
		return value
	default:
		return value
	}
}
