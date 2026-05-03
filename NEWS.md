# aggreCAT (development version)

<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

- #66 update bibliography and refs,
- switch to native pipe unless magrittr needed #70
- sentence case for headings, not title case #70
- Explain ReasonWAgg notation (R) #68
- Rm old caption for ridgeplot (where grouped by method type) #68
- Highlight different example in heatmap interpretation #69
- update weight text to reflect the conceptual difference between user-supplied *weights*, and user-supplied *data* for constructing weights
- reorganise order
- arrange table 1 to match fig 2 best estimate order
- edit ArMean.png to show correct final estimate
- pick bind_rows over map_dfr
- clarify explanations of demonstrated aggregations
- Ensure equations reflect code rebase
- switch to parsing all focal claims to IntervalWAgg example
- conceptual framing + terminology: external weights includes things like ReasonWAgg and QuizWAgg, user-supplied includes LinearWAgg, reserve 'suite' for entire list of options/aggregators in package
- don't namespace aggreCAT functions or data
- delete bespoke code output, just include listing instead
- Better explain ridgeplot in text and in caption
- create ridgeplot function
- rearrange text and tidy code chunks
- copy lua filter used in jss
- map lua filter  commands to latex commands in preamble.tex
- Replace 'claim' with 'forecast'
- syntax errors in cross-refs etc.
- rejig focal claim prep, echo code #68
-  fix equation cross-ref syntax - no square braces, fix final header levels #70
- fix subscript on eqns
- relevel headers for appendices, relabel Appendices with prefix, https://github.com/orgs/quarto-dev/discussions/4581#discussion-4915248
- Put text back into listing 2 and modify caption
- add section labels for cross-refs
- fix listing cross-reference syntax - NO SQUARE BRACES
- simplify agg method description,
- Explain filtering after all judgements
- switch to using data_ratings to exemplify sensitivity to input data
- explain 'name' argument, and dataset prep / description in text
- use focal_claim_108
- Switch to claim 108, originally was using different claim tobecause it had more reasoning categories with 1's. Selecting a sample of categories in Table 2 over comes this issue.
- Tidy Table 2, reasoning data, take sample of categories
- Update user-supplied data text
- cutdown IndIntWAgg text and mv up to judgement-level weights
- Merge worked example code for IntervalWAgg methods togther
- Package data
- #70 equation cross-ref syntax fix
- use lua filter
- insert latex header info into preamble
- control reference output location
- echo and include output
- And eval the ridge plots after #63
- Mention other wrapper functions and revise explanation of rescaling
- Insert fig for reasonwagg #70
- reindent code #70
- conditionally execute fansi colouring #70
- user-supplied weights: weights are calculated by the user, not internally within the function.
- And rm whitespace
- Rebuild / usethis
- Add description of each wrapper fn #69
- echo LinearWAgg example #68
- Restructure worked examples (rm detailed explanations, reorganise) #69
- colour cli output in html output and set message = true #70
- Wrapping table in quarto div seems to address missing caption on first instance of longtable https://github.com/quarto-dev/quarto-cli/issues/1486
- Ensure all tbl captions are at top (set doc level option)
- set number depth
- Fix intro header level
- typos
- reorganise IDEA protocol Box content #67, #72
- #72 reduce IDEA protocol content
- #67 discuss wrapper functions first, including blueprints
- remove redundancy between new package dataset subsection and existing section on tidy workflows. Package datasets section now functions as brief inventory of what ships with package, while tidy aggregation and required input explains the relationship between the user's data and the aggregation function, presupposing mental model of the data, explaining how the aggregation functions expect data to be structured and what they return. Then the worked example illustrates that data with the functions.
- moved detailed content into vignette
- delete dataset documentation
- add longer dataset descriptions manuscript-> vignettes
- add to pkgdown yml
- copy contents of inst/ms/bibliography.bib into new .bib

# aggreCAT 0.0.0.9004

- Plot funs updated
- update Roxygen version in description
- removed unneeded tests


# aggreCAT 0.0.0.9003

- added a `validator` fun in preprocess.
- Added value check for probs outside `0,1` values.
- Updated plotting to removed depricated args.
- removed `experience_weights` ref #6.
- added `globals.R`
- Workflow `test` on updated image.
- `(--compact-vignettes=both)` arg added to build argument
- Update `.Rbuildignore`
- NAMESPACE fix for `margin` in plot function.
- Removed old data objects script.
- Deleted `codecov.yml` for later setup.


# aggreCAT 0.0.0.9002

- update README with ArMean demo and attribution details
- Added `JAGS` check.
- house keeping
- updates and fixes
- PR conflict fix.


# aggreCAT 0.0.0.9001

- Tests updated and addresses some of isse #2 and #4.
- data objects updated.
- Update data objects #2
- Added CI testing


# aggreCAT 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
