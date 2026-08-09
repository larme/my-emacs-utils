# gptel code-study helper: design and migration notes

This document records the goals, observations, decisions, implementation
contract, and remaining limitations for `my-gptel-utils.el`. It is intended to
be sufficient context for moving the helper to another repository without
having to reconstruct the original discussion.

Snapshot date: 2026-08-09.

Current artifacts:

- `my-gptel-utils.el`: implementation.
- `my-gptel-utils-tests.el`: focused ERT tests.
- `my-gpt-utils-notes.md`: this handover document.

At this snapshot, the implementation and tests are untracked files in the
gptel checkout. The unrelated `my-gptel-rewrite-with-buffer-context.el` is not
part of this feature and was deliberately left untouched.

## Original goal

The desired workflow is for interactive source study:

1. Invoke one command from a source buffer.
2. Enter a question in the minibuffer.
3. Send the current source buffer, plus useful related code, as request
   context.
4. Do not insert the response into the source buffer.
5. Display the answer in a separate gptel conversation buffer.
6. Reuse that conversation for later questions about the same source and
   model.
7. Append each new question after the preceding response and let ordinary
   gptel conversation handling insert the next response.

The public entry point is named `my/gptel-ask-buffer`, not
`my/gptel-study-buffer`.

The naming rule is:

- Public functions begin with `my/`.
- Private functions begin with `my/-`.
- Private state follows the same `my/-gptel-ask-*` family.

## What we learned about gptel context

`gptel-context` is a list of context sources. A source can be:

- A live buffer object, meaning the entire live buffer.
- A file-name string, meaning the file contents.
- A buffer or file paired with a specification such as `:lines`, `:bounds`,
  or `:overlays`.

For example:

```elisp
(list source-buffer
      (list "/project/module.py" :lines '((10 . 40) (70 . 90))))
```

Adding a file to `gptel-context` is persistent state, not automatically a
one-request attachment. It remains context until it is removed, the variable
is rebound, or its owning buffer is killed. Therefore, global context is a bad
fit for a source-specific study conversation.

The initial transient `g`/`b` context-selection workflow was also too easy to
misread: selecting an existing context collection did not reliably express
"include the source buffer from which I am asking." The helper consequently
does not rely on transient selection semantics. It installs the source and
automatic relationships explicitly.

The central isolation decision is:

```elisp
(setq-local gptel-context ...)
```

inside the dedicated question buffer. The global value of `gptel-context` is
never modified by this helper.

The question buffer also sets:

```elisp
(setq-local gptel-use-context 'system)
```

The original source buffer is included as a live whole-buffer context. This
means edits made before request serialization are visible without copying the
source text. Related symbol locations normally use line ranges rather than
overlays, which avoids creating and cleaning up visible context overlays in
source buffers.

## User-visible behavior

From a source buffer:

```text
M-x my/gptel-ask-buffer
```

The command reads a non-empty question from the minibuffer. It creates or
reuses a buffer named:

```text
*<model-name>-<source-buffer-name>*
```

Examples:

```text
*gpt-5.6-parser.py*
*claude-sonnet-main.c*
```

The selected gptel backend and model are captured from the source buffer and
installed buffer-locally in the question buffer. Reusing a question buffer
with a different source, backend, or model is rejected instead of silently
mixing conversations.

Only one request or context collection may be active for a question buffer.
The question is appended before collection begins, but `gptel-send` is called
only after context collection completes or times out. If sending fails, the
unsent prompt is rolled back. If the source buffer disappears, the question is
not sent.

The question buffer is displayed with `gptel-display-buffer-action`. The
source buffer is not modified by the answer.

## Collection profiles

There are three profiles:

| Profile | Meaning |
|---|---|
| `source-only` | Include only the live source buffer and preserve manual context. Do not run providers. |
| `smart` | Include the source plus direct dependencies and focused symbol relationships. This is the default. |
| `deep` | Include smart context plus broader references and one-hop call relationships. |

Normal interactive invocation uses `my/gptel-ask-default-profile`, whose
default value is `smart`.

With a prefix argument:

```text
C-u M-x my/gptel-ask-buffer
```

the command uses `deep`.

`source-only` can be made the default through Customize or requested from
Elisp:

```elisp
(my/gptel-ask-buffer 'source-only)
```

## Architecture

The implementation deliberately separates discovery from context mutation:

```text
source buffer + point + profile
              |
              v
      applicable providers
       /       |        \
    xref    generic LSP  language/server plugins
       \       |        /
        context candidates
              |
              v
 canonicalize -> filter -> merge -> score -> budget
              |
              v
 chat-local automatic gptel context
              |
              v
          gptel-send
```

Providers never modify `gptel-context`. They return candidates to a central
manager. This prevents provider ordering from corrupting state and makes it
possible to add later language plugins without teaching each plugin about
gptel's context ownership.

### Context ownership

The question buffer tracks three logical context sets:

```elisp
my/-gptel-ask-base-context
my/-gptel-ask-auto-context
my/-gptel-ask-manual-context
```

- Base context contains the live source buffer.
- Automatic context is replaced on each fresh collection.
- Manual context consists of entries found in the chat-local `gptel-context`
  that were not owned by the previous base or automatic sets.

The installed value is the deduplicated concatenation:

```text
base + automatic + manual
```

This is important: refreshing automatic relationships must not delete context
the user manually added to the question buffer.

## Public plugin API

### Candidate constructor

```elisp
(my/gptel-ask-context-candidate
 SOURCE
 :relation RELATION
 :lines LINES
 :score SCORE
 :origin ORIGIN)
```

`SOURCE` is a live buffer or file name. `LINES` is nil for the whole source, a
single one-based inclusive `(FROM . TO)` range, or a list of such ranges.
`SCORE` defaults to 50. `RELATION` and `ORIGIN` are descriptive metadata used
by the manager and for future diagnostics.

### Registration

```elisp
(my/gptel-ask-register-context-provider
 NAME
 :priority PRIORITY
 :predicate PREDICATE
 :collect COLLECT)
```

Registering the same `NAME` replaces the old provider. Larger provider
priorities run first, but provider priority does not override individual
candidate scores.

`PREDICATE` receives a request and returns non-nil when the provider applies.

`COLLECT` is called as:

```elisp
(COLLECT REQUEST CALLBACK)
```

It must eventually invoke `CALLBACK` with a list of candidates. It may return
a zero-argument cancellation function. A synchronous provider calls the
callback before returning and normally returns nil.

Providers can inspect requests through these public functions:

```elisp
(my/gptel-ask-context-request-source-buffer request)
(my/gptel-ask-context-request-point request)
(my/gptel-ask-context-request-profile request)
(my/gptel-ask-context-request-project-root request)
```

Unregister with:

```elisp
(my/gptel-ask-unregister-context-provider NAME)
```

Minimal synchronous plugin example:

```elisp
(my/gptel-ask-register-context-provider
 'example-language
 :priority 60
 :predicate
 (lambda (request)
   (with-current-buffer
       (my/gptel-ask-context-request-source-buffer request)
     (derived-mode-p 'example-mode)))
 :collect
 (lambda (request done)
   (let ((related-file
          (expand-file-name
           "related.example"
           (my/gptel-ask-context-request-project-root request))))
     (funcall
      done
      (when (file-readable-p related-file)
        (list
         (my/gptel-ask-context-candidate
          related-file
          :relation 'direct-dependency
          :score 75
          :origin 'example-language)))))
   nil))
```

An asynchronous LSP plugin should return a cancellation closure and must make
its callback one-shot. The central manager also guards against duplicate
provider callbacks.

## Built-in providers

The built-ins are registered at load time. Reloading the file replaces them by
name rather than accumulating duplicates.

| Provider | Priority | Applies when | Smart behavior | Additional deep behavior |
|---|---:|---|---|---|
| `python-imports` | 110 | Python mode with an xref backend | Resolve direct imports through xref | Same direct-import behavior |
| `xref` | 100 | Any source with an xref backend | Definition at point | References at point |
| `c-family-lsp` | 90 | C-family buffer using lsp-mode | Include links and clangd/ccls source-header counterpart | Same dependency behavior |
| `ccls` | 80 | Deep profile served by ccls | Not run | One-hop caller and callee trees through `$ccls/call` |
| `lsp` | 70 | Any file buffer using lsp-mode | Implementation and type definition | Standard incoming and outgoing call hierarchy |

Current candidate scores are:

| Relationship | Score |
|---|---:|
| xref definition | 95 |
| source/header counterpart | 90 |
| LSP implementation | 88 |
| C/C++ include | 82 |
| Python import | 80 |
| LSP type definition | 72 |
| ccls caller/callee | 58 |
| standard LSP caller/callee | 55 |
| xref reference | 45 |

Scores order candidates before budgets are applied. They do not change the
text content.

## Why both xref and LSP are used

xref is the portable relationship layer. It can be backed by lsp-mode, Eglot,
TAGS, an Emacs major mode, or another xref implementation. The helper uses it
for definitions and references, allowing useful behavior even when lsp-mode
is not active.

LSP adds relationships not expressed cleanly by portable xref APIs:

- Implementations.
- Type definitions.
- Document links used as resolved include targets.
- Source/header pairing.
- Call hierarchy.
- Server-specific extensions such as `$ccls/call`.

When lsp-mode is the xref backend, xref definitions and references already
become LSP requests. The raw LSP provider consequently does not request those
same relationships again. This avoids obvious duplicate requests.

xref operations may be synchronous, including when lsp-mode implements the
xref backend. The helper therefore keeps xref work focused and capped. Raw LSP
requests are asynchronous and cancellation-aware.

## Python strategy

There is no standard LSP request for "give me this file's complete import
dependency graph." Pyright's documented custom commands also do not expose a
general dependency-closure request. The Python provider therefore combines
syntax discovery with xref resolution rather than depending on a
Pyright-specific private protocol.

The provider:

1. Detects `python-mode`, `python-ts-mode`, or `python-base-mode` descendants.
2. If the buffer already has a tree-sitter parser, traverses the syntax tree
   for `import_statement` and `import_from_statement` nodes.
3. Falls back to line-oriented import recognition if tree-sitter is not
   active or traversal fails.
4. Handles ordinary imports, comma-separated imports, aliases, `from`
   imports, and relative imports.
5. Places point on the final module component, where an LSP-backed xref backend
   can use the surrounding syntax to resolve the full import.
6. Calls `xref-backend-definitions` and proposes the destination as a whole
   direct dependency.

The number of import positions is capped by
`my/gptel-ask-max-python-imports`, currently 12. Project filtering normally
removes standard-library files, virtual environments, and `site-packages`.
The implementation is intentionally useful with Pyright, pylsp, Eglot, or
another xref backend rather than being coupled to one Python server.

## clangd and ccls strategy

The C-family provider recognizes these mode families:

```text
c-mode, c-ts-mode, c++-mode, c++-ts-mode, objc-mode, cuda-mode
```

For any C-family lsp-mode server that advertises document-link support, it
requests:

```text
textDocument/documentLink
```

clangd resolves include directives as document links, so local link targets
become whole-file `include` candidates.

When the active server identifier matches `clangd` or `ccls`, the provider
also requests:

```text
textDocument/switchSourceHeader
```

The corresponding source or header is a high-priority whole-file candidate.

The generic deep LSP provider uses:

```text
textDocument/prepareCallHierarchy
callHierarchy/incomingCalls
callHierarchy/outgoingCalls
```

ccls additionally receives two one-level `$ccls/call` requests, one for
callers and one for callees. Their recursive response trees are searched for
URI-bearing locations. Central deduplication removes overlap with standard
call hierarchy results.

System headers are normally rejected because automatic context defaults to
project-local files only. This is intentional; including the standard library
or SDK graph would consume the context window quickly.

## LSP adapter behavior

The implementation does not require `lsp-mode` when the file is loaded.
Providers become applicable only when:

- `lsp-mode` is active in the source buffer.
- The source visits a file.
- `lsp-request-async` is available.

Standard requests are capability-gated using `lsp-feature?`. If capability
inspection is unavailable, the adapter sends optimistically and treats the
request error callback as a graceful unsupported-method result.

Each request receives a unique cancellation token. When collection is
cancelled or times out, `lsp-cancel-request-by-token` is used when available.
URI and result parsing accepts hash tables, plists, alists, vectors, standard
`Location` objects, and `LocationLink`-style `targetUri` objects.

The adapter currently uses some lsp-mode internal helpers when available:

```elisp
lsp--text-document-identifier
lsp--text-document-position-params
lsp--path-to-uri
lsp--uri-to-path
lsp--workspace-client
lsp--client-server-id
```

Fallback construction exists for document identifiers, positions, and file
URIs. Nevertheless, these internal APIs are a version-sensitive migration
point and should be checked when upgrading lsp-mode.

## Candidate normalization and budgets

The manager applies these steps:

1. Reject invalid candidates and unreadable/dead sources.
2. Canonicalize file identities with expanded/truename paths.
3. Exclude the original source from automatic context because it is already
   whole-buffer base context.
4. Deduplicate candidates by canonical source identity.
5. Merge overlapping or adjacent one-based line ranges.
6. If either candidate requests the whole source, whole-source context wins
   over snippets.
7. Prefer an already visiting, unmodified buffer over rereading its file.
8. Sort merged sources by the highest candidate score.
9. Apply per-source, source-count, and total-character budgets.

Current defaults:

| Variable | Default | Meaning |
|---|---:|---|
| `my/gptel-ask-snippet-radius` | 24 | Lines included before and after a related location |
| `my/gptel-ask-max-auto-sources` | 12 | Maximum automatic sources |
| `my/gptel-ask-max-auto-chars` | 100000 | Approximate total automatic characters |
| `my/gptel-ask-max-source-chars` | 40000 | Maximum characters from one automatic source |
| `my/gptel-ask-max-python-imports` | 12 | Python imports resolved per request |
| `my/gptel-ask-max-xrefs` | 40 | xref locations converted per operation |
| `my/gptel-ask-collection-timeout` | 8 | Seconds before sending with completed partial results |
| `my/gptel-ask-cache-ttl` | 30 | Seconds a collection can be reused; zero disables caching |
| `my/gptel-ask-cache-max-entries` | 32 | Cached collections per source buffer |
| `my/gptel-ask-include-external-files` | `nil` | Whether automatic context outside the project is allowed |

The default excluded-path regexp filters directories named:

```text
.git, .hg, .svn, .venv, venv, node_modules, site-packages,
__pycache__, build, dist
```

These are policy defaults, not protocol rules, and should remain customizable.

Character budgets are an approximation, not a model-token calculation. The
base source buffer and manually added context are intentionally outside the
automatic-provider budget.

## Cache behavior

The cache is buffer-local to the source buffer, not global and not owned by the
question buffer. A cache key contains:

- `buffer-chars-modified-tick` of the source.
- Source point.
- Collection profile.
- Provider-registry generation.
- Sorted active lsp-mode server identifiers.

Cached values are context references and line specifications, not copied file
contents. A live buffer therefore stays live when reused.

Provider registration or unregistration increments the registry generation,
invalidating prior keys. A source edit changes the modified tick. Dead buffers
and unreadable files are removed when cached context is read.

Timed-out partial collections are not cached. Normally completed collections,
including an empty result, are cached.

Clear the cache from either the source or its question buffer with:

```text
M-x my/gptel-ask-clear-context-cache
```

The cache does not observe edits to unrelated dependency files or server-side
workspace changes. The 30-second TTL bounds that staleness.

## Asynchronous lifecycle and failure behavior

The request flow is:

1. Capture source buffer, point marker, project root, backend, model, profile,
   and a generation number.
2. Append the question to the conversation.
3. Mark the conversation as active.
4. Reuse a valid cache entry or run all applicable providers.
5. Aggregate each provider callback exactly once.
6. On normal completion, normalize and optionally cache candidates.
7. On timeout, cancel unfinished providers and keep results from providers
   that already completed.
8. Ignore stale callbacks whose generation no longer matches.
9. Merge automatic context with base and manual context.
10. Call `gptel-send` in the question buffer.
11. Clear the active flag through `gptel-post-response-functions`.

Killing the question buffer cancels collection and releases the saved source
point marker. Provider errors are isolated and behave like empty provider
results. A synchronous send error rolls the newly appended question back.

## gptel implementation dependencies

The helper requires:

```elisp
(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'xref)
(require 'gptel)
(require 'gptel-context)
```

It uses public gptel state such as `gptel-context`, `gptel-backend`,
`gptel-model`, `gptel-use-context`, `gptel-send`, `gptel-mode`, and
`gptel-post-response-functions`.

It also uses these internal or semi-internal gptel interfaces:

```elisp
gptel--sanitize-model
gptel--model-name
gptel--request-alist
gptel-fsm-info
gptel-prompt-prefix-string
gptel-response-separator
```

Those calls are the main gptel-version compatibility surface. When moving to
a repository that tracks a different gptel release, inspect them first.

## Moving to another repository

Copy both implementation and tests:

```text
my-gptel-utils.el
my-gptel-utils-tests.el
```

Keep this note alongside them if possible.

The destination must place gptel and this utility on `load-path`. A minimal
configuration is:

```elisp
(add-to-list 'load-path "/path/to/new/repository")
(require 'my-gptel-utils)
(global-set-key (kbd "C-c q") #'my/gptel-ask-buffer)
```

If the destination is packaged with a package manager, declare gptel as a
dependency. `lsp-mode` is optional at load time and should be an optional
integration dependency. xref and project are built into supported modern
Emacs versions.

The implementation was developed and byte-compiled under GNU Emacs 30.2.
Before claiming support for older Emacs versions, run the tests there and
check APIs such as the predicate argument accepted by `find-buffer-visiting`.

Run tests from the directory containing the two files:

```sh
emacs --batch -Q -L . \
  -l my-gptel-utils-tests.el \
  -f ert-run-tests-batch-and-exit
```

Byte-compile both files:

```sh
emacs --batch -Q -L . \
  -f batch-byte-compile \
  my-gptel-utils.el my-gptel-utils-tests.el
```

The current snapshot passes 11 ERT tests covering:

1. Provider replacement, ordering, and unregistration.
2. Line-range normalization and merging.
3. Candidate deduplication and exclusion of the base source.
4. Preservation of manually added context.
5. Python import-position discovery.
6. LSP URI, location, and line-window conversion.
7. Mocked generic asynchronous LSP collection.
8. Document-link filtering to local whole-file candidates.
9. Immediate source-only collection and send.
10. Preservation of completed results when another provider times out.
11. Cache reuse and invalidation after the source revision changes.

At the snapshot, byte compilation produced no warnings, `check-parens` and
`checkdoc` passed, and whitespace checks were clean.

## Known limitations and deliberate boundaries

### Live LSP verification is still required

`lsp-mode` was not installed in the development environment. LSP behavior was
verified against the current documented/source interfaces and mocked in ERT,
but the helper has not yet been exercised end-to-end against live Pyright,
clangd, or ccls processes.

The first integration work in the destination repository should create tiny
Python and C/C++ fixture projects and observe the exact decoded result shapes
from the installed lsp-mode version.

### The raw LSP adapter is lsp-mode-specific

xref behavior remains portable and can work with Eglot. The richer raw LSP
providers currently require lsp-mode. An Eglot adapter should be a separate
provider using the same candidate contract, not conditionals scattered
through the existing manager.

### Dependency collection is intentionally shallow

The implementation adds direct Python imports, direct resolved C/C++ include
links, a source/header counterpart, and one-hop call relationships. It does
not recursively traverse the entire import/include/call graph. Recursive
closure would be expensive, noisy, and likely to overflow the model context.

### Whole dependency files may be skipped

Python imports, C/C++ includes, and source/header counterparts are proposed as
whole files. If a file exceeds `my/gptel-ask-max-source-chars`, it is skipped
rather than automatically truncated. A future plugin could identify useful
definitions within a large dependency and return snippets instead.

### Project-local filtering can omit useful SDK code

External files, system headers, standard-library modules, virtual
environments, and generated/build directories are excluded by default. Users
can set `my/gptel-ask-include-external-files` or customize the excluded regexp,
but any broader policy should still be budgeted.

### xref can block synchronously

xref definition/reference calls can block Emacs while the backend responds.
The limits reduce result processing but cannot interrupt a synchronous backend
request. Broad asynchronous graph work belongs in LSP providers.

### Conversation buffer naming can collide

The name uses model plus `buffer-name`, not an absolute path. Two live source
buffers with the same name from different projects can therefore target the
same candidate chat name. The ownership check rejects mixing them, but a
future revision could add a project name or short path hash to avoid the
collision.

### Cache invalidation is intentionally simple

The cache follows source edits, point, profile, providers, server IDs, and TTL.
It does not subscribe to LSP workspace events or dependency-buffer modified
ticks. Manual clearing and the short TTL are the current safeguards.

### Automatic budgets do not cap base/manual context

The complete source buffer is always base context, and manual context remains
under user control. Very large source buffers or manually added files can
still exceed a model context window. That is a conscious preservation of the
phase-one behavior, but may need a separate user-facing warning later.

## Decisions that should remain stable during the move

- Keep `my/gptel-ask-buffer` as the main public command.
- Keep public/private function naming as `my/` and `my/-`.
- Never modify global `gptel-context`.
- Keep the original source as live whole-buffer base context.
- Keep answers in the dedicated gptel conversation buffer.
- Reuse the conversation and append later questions normally.
- Preserve manually added chat-local context across automatic refreshes.
- Keep providers pure with respect to `gptel-context`; only the manager installs
  context.
- Use xref as the portable relationship layer and optional LSP providers for
  richer relationships.
- Avoid duplicate raw LSP definition/reference requests when xref already
  covers those relationships.
- Keep automatic dependency expansion shallow, scored, filtered, and budgeted.
- Fail gracefully when lsp-mode, a capability, a server extension, or a
  related file is unavailable.
- Do not make lsp-mode a hard requirement merely to use the source-only or xref
  workflows.

## Recommended next work after migration

1. Add fixture-based live integration tests for Pyright, clangd, and ccls.
2. Verify all private gptel and lsp-mode API calls against the destination's
   pinned versions.
3. Add an optional diagnostic command that displays selected candidates,
   relations, scores, sizes, and rejection reasons before sending.
4. Consider adding an Eglot raw-protocol provider while keeping the same public
   candidate API.
5. Consider snippet extraction for oversized direct dependencies.
6. Consider collision-free conversation names using project identity.
7. If real token accounting becomes available, replace approximate character
   budgets without changing provider contracts.

## Protocol and implementation references

- gptel context implementation:
  <https://github.com/karthink/gptel/blob/master/gptel-context.el>
- gptel request/context variables:
  <https://github.com/karthink/gptel/blob/master/gptel-request.el>
- lsp-mode implementation and xref integration:
  <https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-mode.el>
- Language Server Protocol specification:
  <https://github.com/microsoft/language-server-protocol/blob/gh-pages/_specifications/lsp/3.18/specification.md>
- clangd protocol extensions:
  <https://clangd.llvm.org/extensions>
- clangd LSP server implementation:
  <https://github.com/llvm/llvm-project/blob/main/clang-tools-extra/clangd/ClangdLSPServer.cpp>
- ccls repository and extension documentation:
  <https://github.com/MaskRay/ccls>
- ccls request handlers:
  <https://github.com/MaskRay/ccls/blob/master/src/messages/message_handler.cc>
- Pyright extension command manifest:
  <https://github.com/microsoft/pyright/blob/main/packages/vscode-pyright/package.json>

## Porting acceptance checklist

- [ ] `my-gptel-utils.el` and `my-gptel-utils-tests.el` are copied together.
- [ ] `gptel` and `gptel-context` load successfully before the utility.
- [ ] The utility byte-compiles without warnings on the destination Emacs.
- [ ] All ERT tests pass.
- [ ] `M-x my/gptel-ask-buffer` asks in the minibuffer and leaves the source
      unchanged.
- [ ] The question buffer is named `*<model>-<source-buffer>*` and is reused.
- [ ] Its `gptel-context` is buffer-local and contains the live source.
- [ ] Global `gptel-context` remains unchanged.
- [ ] A second question is appended after the prior response.
- [ ] Manual chat-local context survives automatic recollection.
- [ ] `source-only`, `smart`, and prefix-selected `deep` behave distinctly.
- [ ] xref definitions work without lsp-mode.
- [ ] A live Python server resolves direct local imports.
- [ ] clangd/ccls document links resolve local includes.
- [ ] clangd/ccls source-header switching works.
- [ ] Deep call hierarchy returns callers/callees without unbounded expansion.
- [ ] System, virtual-environment, vendor, and build files are filtered by
      default.
- [ ] Timeout sends with already completed provider results.
- [ ] Killing the question buffer cancels outstanding collection.
- [ ] Cache reuse and `M-x my/gptel-ask-clear-context-cache` work.
