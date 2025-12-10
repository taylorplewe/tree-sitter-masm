package tree_sitter_masm_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_masm "github.com/taylorplewe/tree-sitter-masm/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_masm.Language())
	if language == nil {
		t.Errorf("Error loading masm grammar")
	}
}
