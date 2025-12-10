#ifndef TREE_SITTER_MASM_H_
#define TREE_SITTER_MASM_H_

typedef struct TSLanguage TSLanguage;

#ifdef __cplusplus
extern "C" {
#endif

const TSLanguage *tree_sitter_masm(void);

#ifdef __cplusplus
}
#endif

#endif // TREE_SITTER_MASM_H_
