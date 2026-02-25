#import "utils.typ": (
  and_then, error, global-name, global-prefix, hash, map, map_or,
)

#let env-state(identifier) = state(global-name("state", "env", identifier), none)
#let env-items-state(identifier) = state(global-name("state", "items", identifier), ())
#let env-locations-state(identifier) = state(global-name("state", "locs", identifier), (:))
#let env-group-items-state(identifier, group) = state(
  global-name("state", "items", identifier, "group", group),
  (),
)

#let parse-exercism-target(target) = {
  if type(target) != label {
    return none
  }
  let fields = repr(target).replace("<", "").replace(">", "").split(":")
  let prefix = fields.at(0, default: none)
  let kind = fields.at(1, default: none)
  let env = fields.at(2, default: none)
  let item-hash = fields.at(3, default: none)
  if (
    prefix != global-prefix
      or (kind != "question" and kind != "solution")
      or env == none
      or item-hash == none
  ) {
    return none
  }
  (kind: kind, env: env, hash: item-hash)
}

#let opposite-fallback-location(target) = {
  let parsed = parse-exercism-target(target)
  if parsed == none {
    none
  } else {
    let locs = env-locations-state(parsed.env)
    let final = locs.final()
    final.at(parsed.hash, default: none)
  }
}

#let display-ordinal(pattern, ordinal) = {
  if pattern == none {
    none
  } else if type(pattern) == function {
    pattern(ordinal)
  } else {
    numbering(pattern, ordinal)
  }
}

/// Create a link to the opposite exercise element.
///
/// Internal exercism labels can remain unresolved in low-iteration layouts, so
/// this falls back to a stable declaration anchor.
#let opposite-link(
  target,
  body,
  strict: false,
) = {
  if strict {
    return link(target, body)
  }
  let parsed = parse-exercism-target(target)
  let fallback = opposite-fallback-location(target)
  if fallback != none {
    link(fallback, body)
  } else if parsed != none {
    body
  } else {
    link(target, body)
  }
}

/// Create a reference to the opposite exercise element.
///
/// If the internal target isn't stable yet, the caller-provided fallback body
/// is linked to a stable declaration anchor.
#let opposite-ref(
  target,
  body,
  strict: false,
) = {
  if strict {
    return ref(target)
  }
  let parsed = parse-exercism-target(target)
  let fallback = opposite-fallback-location(target)
  if fallback != none {
    link(fallback, body)
  } else if parsed != none {
    body
  } else {
    ref(target)
  }
}

/// Create a new exercise environment.
///
/// Returns the function to be used when creating instances of this
/// environment. If two arguments are provided when called, then the first is
/// the question and the second the solution. If three arguments are provided,
/// then the first is the title, the second the question, and the third the
/// solution.
/// -> function
#let new(
  /// A unique key for the environment.
  /// -> str
  identifier,
  /// The environment's supplement.
  ///
  /// If a function is specified, it is passed the environment's body and
  /// should return content.
  /// -> none | content | function
  supplement: none,
  /// How to number the environment. Accepts a #numbering-type.
  /// -> none | str | function
  numbering: "1",
  /// Whether to further separate instances of this environment into groups
  /// based on its location in the document.
  ///
  /// See @group for more information.
  /// -> none | function
  group: none,
) = {
  return (..args) => {
    // Get the exercise's title, question, and solution.
    let args = args.pos()
    let item = if args.len() == 2 {
      (title: none, question: args.at(0), solution: args.at(1))
    } else if args.len() == 3 {
      (title: args.at(0), question: args.at(1), solution: args.at(2))
    } else {
      error("expected either two or three arguments")
    }

    // Unique key for the exercise.
    item.hash = hash(item)

    // Store the exercise in environment/group-local state.
    context {
      let group = map(group, x => x(here()))
      let grouped = group != none
      let stored-item = item
      let item-loc = here()
      stored-item.loc = item-loc

      env-state(identifier).update(old => {
        if old == none {
          (supplement: supplement, numbering: numbering, group: grouped)
        } else {
          old
        }
      })

      env-locations-state(identifier).update(locs => {
        locs.insert(stored-item.hash, item-loc)
        locs
      })

      if group == none {
        env-items-state(identifier).update(items => {
          items.push(stored-item)
          items
        })
      } else {
        env-group-items-state(identifier, group).update(items => {
          items.push(stored-item)
          items
        })
      }
    }

    // Make a label for the exercise attach to this fake metadata which will
    // get redirected to the actual element later on.
    metadata((
      prefix: global-prefix,
      kind: "env",
      env: identifier,
      hash: item.hash,
    ))
  }
}

/// Type signature of the function expected in @new.group.
/// -> str
#let group(
  /// -> location
  location,
) = {}

/// Display all the collected questions with the given formatting.
///
/// Requires ```typc context```.
/// -> content
#let questions(
  /// Key of the environment to use.
  /// -> str
  identifier,
  /// How to format each question.
  ///
  /// See @formatting for more information.
  /// -> function
  formatting,
  /// If @new.group was specified when creating the environment, then a
  /// specific instance of the value returned by @new.group must be given.
  /// -> none | str
  group: none,
) = {
  // Get information about this env.
  let env-current = env-state(identifier).get()
  let env-final = env-state(identifier).final()
  let env = if env-final != none { env-final } else { env-current }
  if env == none {
    // Do nothing if this env doesn't exist.
    return
  }

  // Error out if group is not specified when required.
  if env.group and group == none {
    error("env '" + identifier + "' requires a group to be specified")
  }
  let group = and_then(env.group, group)

  // Unique figure kind for this env.
  let kind = global-name(
    "kind",
    map_or(group, x => identifier + "#" + group, identifier),
    "question",
  )

  // Undo show-set rules on figures.
  show figure.where(kind: kind): set align(start)
  show figure.where(kind: kind): set block(breakable: true)

  let items = if env.group {
    let items-state = env-group-items-state(identifier, group)
    let current = items-state.get()
    let final = items-state.final()
    if final.len() >= current.len() { final } else { current }
  } else {
    let items-state = env-items-state(identifier)
    let current = items-state.get()
    let final = items-state.final()
    if final.len() >= current.len() { final } else { current }
  }

  // Layout each of the questions.
  for (index, item) in items.enumerate() [
    #show figure.where(kind: kind): it => {
      let number = display-ordinal(it.numbering, index + 1)
      formatting(
        it.body,
        it.supplement,
        number,
        map(it.caption, x => x.body),
        label(global-name("solution", identifier, item.hash)),
      )
    }
    #figure(
      item.question,
      kind: kind,
      supplement: env.supplement,
      numbering: env.numbering,
      placement: none,
      caption: item.title,
      gap: 0em,
      outlined: false,
    )
    #label(global-name("question", identifier, item.hash))
  ]
}

/// Display all the collected solutions with the given formatting.
///
/// Requires ```typc context```.
/// -> content
#let solutions(
  /// Key of the environment to use.
  /// -> str
  identifier,
  /// How to format each solution.
  ///
  /// See @formatting for more information.
  /// -> function
  formatting,
  /// If @new.group was specified when creating the environment, then a
  /// specific instance of the value returned by @new.group must be given.
  /// -> none | str
  group: none,
) = {
  // Get information about this env.
  let env-current = env-state(identifier).get()
  let env-final = env-state(identifier).final()
  let env = if env-final != none { env-final } else { env-current }
  if env == none {
    // Do nothing if this env doesn't exist.
    return
  }

  // Error out if group is not specified when required.
  if env.group and group == none {
    error("env '" + identifier + "' requires a group to be specified")
  }
  let group = and_then(env.group, group)

  // Unique figure kind for this env.
  let kind = global-name(
    "kind",
    map_or(group, x => identifier + "#" + group, identifier),
    "solution",
  )

  // Undo show-set rules on figures.
  show figure.where(kind: kind): set align(start)
  show figure.where(kind: kind): set block(breakable: true)

  let items = if env.group {
    let items-state = env-group-items-state(identifier, group)
    let current = items-state.get()
    let final = items-state.final()
    if final.len() >= current.len() { final } else { current }
  } else {
    let items-state = env-items-state(identifier)
    let current = items-state.get()
    let final = items-state.final()
    if final.len() >= current.len() { final } else { current }
  }

  // Layout each of the solutions.
  for (index, item) in items.enumerate() [
    #show figure.where(kind: kind): it => {
      let number = display-ordinal(env.numbering, index + 1)
      formatting(
        it.body,
        it.supplement,
        number,
        map(it.caption, x => x.body),
        label(global-name("question", identifier, item.hash)),
      )
    }
    #figure(
      item.solution,
      kind: kind,
      supplement: env.supplement,
      numbering: none,
      placement: none,
      caption: item.title,
      gap: 0em,
      outlined: false,
    )
    #label(global-name("solution", identifier, item.hash))
  ]
}

/// Type signature of the function expected in @questions.formatting and
/// @solutions.formatting.
/// -> content
#let formatting(
  /// -> content
  body,
  /// -> none | content
  supplement,
  /// -> none | content
  number,
  /// -> none | content
  title,
  /// Label pointing to the accompanying solution (question) of the current
  /// question (solution).
  /// -> label
  opposite,
) = {}

/// Show rule to enable correct references to questions.
///
/// Should be applied with ```typ #show ref: show-ref``` at the start of the
/// document.
/// -> ref
#let show-ref(
  /// -> ref
  it,
  /// Whether to always use an exact redirected reference for exercism labels.
  /// This may require more layout iterations than the fast fallback path.
  /// -> bool
  strict: false,
) = {
  // Do we have a sequence?
  if it.element == none or it.element.func() != [].func() {
    return it
  }

  // Is the last child metadata?
  let last = it.element.children.at(-1, default: none)
  if last == none or last.func() != metadata {
    return it
  }

  // Fast path for new metadata format: render a stable ref directly to the
  // exercise declaration location (no query/forward-label dependency).
  let value = last.value
  if (
    type(value) == dictionary
      and value.at("prefix", default: none) == global-prefix
      and value.at("kind", default: none) == "env"
  ) {
    let env-id = value.at("env", default: none)
    let hash = value.at("hash", default: none)
    if strict and env-id != none and hash != none {
      return ref(
        label(global-name("question", env-id, hash)),
        supplement: it.supplement,
        form: it.form,
      )
    }
    let env-current = if env-id != none { env-state(env-id).get() } else { none }
    let env-final = if env-id != none { env-state(env-id).final() } else { none }
    let env = if env-final != none { env-final } else { env-current }
    let ordinal = none
    if env != none and hash != none and not env.group {
      let items-state = env-items-state(env-id)
      let current = items-state.get()
      let final = items-state.final()
      let items = if final.len() >= current.len() { final } else { current }
      for (index, item) in items.enumerate() {
        if item.hash == hash {
          ordinal = index + 1
        }
      }
    }
    let simple-form = it.form == none or it.form == "normal"
    let supplement = if it.supplement == auto and env != none { env.supplement } else { it.supplement }
    let question-target = if env-id != none and hash != none {
      label(global-name("question", env-id, hash))
    } else { none }
    if env != none and ordinal != none and simple-form and type(supplement) != function {
      let number = display-ordinal(env.numbering, ordinal)
      let body = if supplement == none {
        number
      } else if number == none {
        supplement
      } else {
        [#supplement #number]
      }
      return link(it.element.location(), body)
    }

    // Fall back to the old redirect if we can't render a simple stable ref.
    if env-id != none and hash != none {
      return ref(
        label(global-name("question", env-id, hash)),
        supplement: it.supplement,
        form: it.form,
      )
    }
    return it
  }

  // Backwards-compatible path for older metadata format.
  if type(value) != str {
    return it
  }
  let fields = value.split(":")
  let prefix = fields.at(0, default: none)
  if prefix == none or prefix != global-prefix or fields.at(1) != "env" {
    return it
  }

  // Redirect reference target to the correct element.
  let (env, hash) = (fields.at(2), fields.at(3))
  ref(
    label(global-name("question", env, hash)),
    supplement: it.supplement,
    form: it.form,
  )
}
