---
title: Twin paradox
description: Explaining the twin paradox from the perspective of the other twin.
published: 2023-05-10
toc: true
include-math: true
include-bibliography-stylesheet: true
---

Discusses the twin paradox in special relativity [@carroll2022; @susskind2017].
In particular, discusses what happens in the reference frames of the traveling twin,
which is often glossed over in many popular treatments of the topic.

## The "paradox" {#intro}

Let's say there are twins.  One of them stays on Earth.
The other twin travels to a star near the speed of light and comes back.
Due to [time dilation], time for the twin traveling to the star
passes more slowly from the inertial frame of the twin staying on Earth.
But for the traveling twin, it is the twin staying on Earth which is moving
and whose time is passing more slowly.

Both twins supposedly observe time flowing more slowly for the other twin.
But when they meet up back on Earth and compare their clocks,
they can't both be right, can they?

[time dilation]: ../../spacetime-diagrams#time-dilation

## Standard popular treatment {#popular-treatment}

Indeed, when the twins meet up back on Earth and compare their clocks,
they would see that time had passed more slowly for the twin who traveled to the star.
So the twin paradox is not truly a paradox.

Most popular treatments of the twin paradox point out that the traveling twin
must accelerate and does not stay in a single inertial frame.
It would be incorrect to conclude that time for the twin on Earth
consistently passes slowly from the perspective of the traveling twin.
Some may even point out that for any world line which starts and ends
at the same two events, the straight one will [experience the most time],
which in this case would be the world line for the twin who stays on Earth.
Below is a [spacetime diagram] for these world lines from
the inertial frame of Earth.

![World lines for the twins](/diagrams/article/relativity/paradox/twin/worldlines.svg)

[experience the most time]: ../../proper-time/#longest

[spacetime diagram]: ../../spacetime-diagrams

These explanations are absolutely correct.
However, I have always found them highly unsatisfactory.
It's not as if the traveling twin cannot observe anything,
but popular treatments of the twin paradox
almost never discuss anything about what they see.

In addition, let's say the traveling twin only accelerates when they depart from Earth,
when they turn around at the star, and when they stop at Earth.
For most of the trip to the star, they would be in a single inertial frame,
and from this inertial frame, time for the twin on Earth will indeed pass more slowly.
Likewise for the trip back from the star to Earth.
And yet at the end it is the time for the traveling twin which had passed more slowly.
What happened during the acceleration to end up in this state?

Even worse, let's say there are two twins who travel to and back from two different stars
that are at the same distance but in opposite directions.
For both twins, time for the other twin should be passing more slowly
during the outbound trip and the inbound trip,
and yet when they meet up back at Earth,
it will turn out that the same amount of time had passed for both of them.
How could this be?

Is it the case that special relativity is unable to infer anything about
what an observer sees if they do not permanently stay in a single inertial frame?
Do we need to reach for general relativity to figure any of this out?

## The other twin's inertial frames {#other-frames}

No, we do not need general relativity for this particular problem.
We would need general relativity if spacetime is curved
or if mass or energy affects spacetime significantly,
neither of which is the case here.
Special relativity suffices to explain what happens from
the other twin's perspective.

First, let us take a look at the spacetime diagram of the world lines of the twins
in the inertial frame where the traveling twin is stationary during their outbound trip.
This diagram is below, with the origin set at the event where
the traveling twin arrives at the star.

![World lines in the inertial frame for the outbound trip](/diagrams/article/relativity/paradox/twin/outbound-frame.svg)

Interestingly, much less than half of the total time has passed for the twin
who remains on Earth in the inertial frame above by the time
the traveling twin arrives at the star.
What about the other way around, when the traveling twin departs from the star
and heads back to Earth?  This diagram is below, likewise with the origin
set at the event where the traveling twin departs from the star.

![World lines in the inertial frame for the inbound trip](/diagrams/article/relativity/paradox/twin/inbound-frame.svg)

Similarly to the perspective from the outbound trip, much less than half the time
passes for the twin from the inertial frame where the traveling twin is stationary
during their inbound trip.  Also note that the time on Earth is significantly _after_
the halfway point when the traveling twin departs from the star.

What does this mean?  Time _does_ pass more slowly for the twin on the Earth in the
inertial frames for the other twin while they are traveling to and from the star.
However, there is a whole chunk of time in the middle for the twin on the Earth
which is skipped while the traveling twin is in these inertial frames.
Said another way, while the traveling twin is on their way to the star,
the twin on Earth ages slowly in the traveling twin's inertial frame,
but the twin on Earth ages suddenly when the other twin turns around at the star,
and the twin on Earth ages slowly again while the traveling twin heads back to Earth.

We can see this as well with the spacetime diagram in Earth's inertial frame.
If we map the \(x\) axes from the previous two diagrams into Earth's inertial frame,
we will get the spacetime diagram below.
We can see between these axes the same chunk of time for the twin on the Earth
which does not pass in the inertial frames for the traveling twin while they are
traveling to the star and coming back from the star.

![World lines from Earth with the \(x\) axes from the traveling twin](/diagrams/article/relativity/paradox/twin/worldlines-axes.svg)

This explains why it seemed the twin paradox was paradoxical.
We correctly reasoned that time will pass slowly for both twins,
but it's too easy for us to assume that a "now" is in every inertial frame,
and not realize that there's a big gap in the time on Earth
between the inertial frames for the traveling twin when they turn around.

## What the other twin sees {#other-observations}

In the previous section, we saw that from the inertial frames for the traveling twin,
time travels more slowly on Earth, then there's a big skip during the short period
where the traveling twin turns around at the star, and then time travels more slowly
on the Earth again while the traveling twin returns to Earth.  But light is still
being emitted from Earth during the time skip.
Does this mean that the traveling twin will be incinerated at the star
as they turn around as all the during this time skip arrives at the star all at once?

No, the traveling twin will not be vaporized by an almost infinite spike of light.
You may or may not have noticed that I said things like "time passes slowly for
the other twin" in the previous sections.  I was careful not to say something like
"one twin _sees_ time passing slowly for the other twin", because they are actually
not the same thing.  Light needs to travel from one twin to another for them to
see each other, so the _observed_ passage of time ends up being different from
the passage of time.

So what does the traveling twin observe?
We can see what it would be like by plotting the light rays leaving
the twin on Earth in a spacetime diagram, and look at how they
meet the traveling twin.

![Light from twin on Earth](/diagrams/article/relativity/paradox/twin/worldlines-light.svg)

Looking at how the light rays reach the traveling twin,
we can see that the traveling twin on their outbound leg will observe time on Earth
passing even more slowly than would be predicted by time dilation.
In contrast, on the inbound leg, they will observe time on Earth passing by very quickly,
and the twin on Earth would have aged a lot more once they meet up again.
The light rays from Earth do not all arrive at the traveling twin in a single instant,
so they will not be incinerated.

## When both twins travel {#both-twins}

![World lines when both twins travel with their \(x\) axes](/diagrams/article/relativity/paradox/twin/both-travel-axes.svg)

## Conclusion {#conclusion}

## See also

*   [Spacetime diagrams](/article/physics/relativity/spacetime-diagrams);
    visualizing relativistic world lines
*   [Proper time](/article/physics/relativity/proper-time/);
    how moving objects experience time

## References
