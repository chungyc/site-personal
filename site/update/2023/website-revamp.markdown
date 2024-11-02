---
title: Rebuilding the web site
published: 2023-02-01
---

I am completely rebuilding my web site from scratch.
The previous one was becoming hopelessly out of date due to lack of updates.
I also decided its structure didn't satisfy what I wanted from my personal web site.

There were two problems with the previous web site that I want to deal with:

*   It was structured more like a blog instead of a more general web site.
    While this was fine in its early days when I regularly posted updates,
    it no longer serves my purpose given how infrequently I write posts.
    I want the new web site to be more informational instead of an online diary.

*   An even bigger problem is that I am unwilling and unable to spend the time
    to maintain the [Drupal] installation serving the previous web site.
    I used to be very hands-on with software maintenance a long time ago,
    but I no longer have the time nor willingness to keep software
    up to date manually.  This is a big problem because security vulnerabilities
    have been left unpatched, and I really need to do something about them.

I am rebuilding the new site using [Hakyll].  It is a static site generator,
which means I would not have to regularly update software in a server
to keep it free from security vulnerabilities.  I'm also a fan of Haskell,
and Hakyll is written in Haskell, so this seemed to be a good opportunity
to use Haskell for something I use.  It also means it would be a lot
easier to extend the site to support various things I would like it to include.

I'm even using this as an opportunity to use a Haskell-based CSS preprocessor, [Clay].
This also means that the style is extremely minimal,
since I don't have the experience or time to create beautiful styles for web sites.
Surprisingly, I'm pretty happy with the minimal look; I might keep it the way it is now,
or I might change it little by little until it is a lot more polished.

[Clay]: http://fvisser.nl/clay/
[Drupal]: https://drupal.org/
[Hakyll]: https://jaspervdj.be/hakyll/
