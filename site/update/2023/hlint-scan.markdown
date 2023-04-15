---
title: Scan code with HLint
description: About the `v1` release of a code scanning GitHub action using HLint.
published: 2023-04-15
---

I have just put on the finishing touches to the `v1.0.0` release of
the ["Scan code with HLint"] GitHub action.  It is a code scanning action
which uploads suggestions from [HLint] to [GitHub code scanning dashboards].

It was an interesting journey to get to this point.
I had added [SARIF support] to HLint, and it should have been a simple matter
of running HLint and uploading its SARIF output to GitHub.
In fact, this is what the action [originally did].

However, it didn't quite work nicely enough with GitHub:

*   [Hardening security] for the inputs was not completely feasible
    with what it was doing as a [composite action].

*   GitHub does this weird thing with text messages where it didn't
    treat them as completely text as it should have been doing,
    nor as completely [Markdown], but some very restricted and undocumented
    subset of Markdown.  This resulted in poor formatting of the suggestions
    from HLint.

*   GitHub doesn't know what to do with `./` in paths.

There was no way I was going to lose my sanity trying to deal with these
using shell commands.  I briefly considered reimplementing this as
a [JavaScript action] in [TypeScript] or [PureScript].  But this is
an action for scanning [Haskell] code, so why not?  I decided to
rewrite it as a [Docker container action] in Haskell.

This had a lot of advantages such as the implementation being
far superior than what a shell script could be like
and easily being able to do proper testing.
After some additional work to reduce the Docker image size
it was using so that loading it took 5 seconds instead of 30 seconds,
[here we are today].

I also did some work so that [OSSF Scorecard], which is a code scanning action
which identifies how a project could improve its security practices,
to [recognize this as a code scanning action], in addition to getting it
to [rightfully recognize] that property-based testing in Haskell
is what is sometimes called fuzzing in other languages.

["Scan code with HLint"]: https://github.com/marketplace/actions/scan-code-with-hlint

[Hlint]: https://github.com/ndmitchell/hlint

[GitHub code scanning dashboards]: https://docs.github.com/en/code-security/code-scanning/automatically-scanning-your-code-for-vulnerabilities-and-errors/about-code-scanning

[SARIF support]: https://github.com/ndmitchell/hlint/pull/1482

[originally did]: https://github.com/haskell-actions/hlint-scan/blob/95d44004b766f20119906292cf8b2bf2eb0b1e2f/action.yaml

[Hardening security]: https://docs.github.com/en/actions/security-guides/security-hardening-for-github-actions

[composite action]: https://docs.github.com/en/actions/creating-actions/creating-a-composite-action

[Markdown]: https://www.markdownguide.org/

[JavaScript action]: https://docs.github.com/en/actions/creating-actions/creating-a-javascript-action

[TypeScript]: https://www.typescriptlang.org/

[PureScript]: https://www.purescript.org/

[Haskell]: https://www.haskell.org/

[Docker container action]: https://docs.github.com/en/actions/creating-actions/creating-a-docker-container-action

[here we are today]: https://github.com/haskell-actions/hlint-scan

[OSSF Scorecard]: https://securityscorecards.dev/

[recognize this as a code scanning action]: https://github.com/ossf/scorecard/pull/2846

[rightfully recognize]: https://github.com/ossf/scorecard/pull/2843
