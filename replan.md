# Replanning the report structure

Based around the three boxes from the whiteboard.

## Box1
Introducing CC at the same time as side effects feels like it comes from out of nowhere, so my best guess is repeat the setting-motivation twice.

### Scene setting 1
What are side effects in programming?
What are algebraic effects?
What are effect systems? Could namedrop some.

### Motivation 1
Why are side effects unavoidable?
Why do we especially care about them in functional?
When & why are effects problematic?
EXAMPLE - specific problem of some leak (ideally in an effect system). Could be based around the file lambda snippet used everywhere in Scaladays and the docs.

### Scene setting 2
What is capture checking?
What safety guarantees does capture checking provide?

### Motivation 2
CC applicability to the Motivation 1 example as a fix. And that it's compatible with most current effect system libraries (turbolift is an exception, but still finicky).

### Contributions + Outline
Will be done last, retrospectively fitted to the rest

## Box2
### Background
My background section from interim. I'm considering moving a couple parts on the implementation plans outside of this and down to content. E.g. what I called 'Unifying the capability' and impl details like 'this will be a shallow embedding' that uses scala givens.

### Content
My project chapter previously had
  - Base types
  - Validating the principles with examples
  - Standard effects
    - All of them in subheadings
  - Trampolining
Evaluation was to be done as an entire separate chapter.

Attempt at restructuring is below. Is this following the intro/stuff/conclusion fractal pattern?

### Design
- Unifying the capability

#### Basic types OLD
Intro- These are the types this paper is based around?

Main body- effect and capability scala traits. Check it provides the guarantees we think

Conc- We formally defined effects and capabilities!

The background stuff I wanted to lift out would go here. Give the mixin trait their own Intro/Main/Conc.

#### Validating
All my previous code was agnostic of standard-defined effects. Can fit under the main body of basic types.

#### Standard effects
Each standard effect should be follow:

Intro- Attempt to demo leaks under existing practice. Could prove tricky (?) but important to do.

Main body- Discuss impl. Evaluate it (benchmark/safety/expressivity/user code quality).

Conc- Summary, or skip if short enough

#### Trampolines
Unsure whether this should be its own section or interleaved throughout on applications.
If the former, keep trampolining background exclusively here. If latter, lift to the background above.

### Related work
My old 'state of the art' section.

## Box3
### Summary
To be done after I have all my content sub-summaries. Will go something like 'CC is too restrictive in many places. Requires developer discipline of tagging everything interesting as a capability. Overall very experimental.'

### Re-example (Remotivation?)
Show what the initial example looks like under the implementation

### Future work + Closing thoughts
Less important. To be done nearer the end.