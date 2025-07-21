<TeXmacs|2.1.4>

<style|<tuple|generic|british>>

<\body>
  <doc-data|<doc-title|Scheme functions specific to
  Scheme\UMan>|<doc-author|<author-data|<\author-affiliation>
    (Revision of <date|>)
  </author-affiliation>>>>

  <\wide-tabular>
    <tformat|<cwith|1|6|1|2|cell-bsep|0.5fn>|<cwith|1|6|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|2|cell-bsep|0.5fn>|<cwith|1|1|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|2|cell-bsep|0.5fn>|<cwith|1|1|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|1|cell-row-span|1>|<cwith|1|1|1|1|cell-col-span|2>|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|2|cell-tborder|1ln>|<cwith|1|1|1|2|cell-bborder|1ln>|<cwith|1|1|1|2|cell-lborder|0ln>|<cwith|1|1|1|2|cell-rborder|0ln>|<cwith|6|6|1|2|cell-bsep|0.5fn>|<cwith|6|6|1|2|cell-tsep|0.5fn>|<cwith|2|-1|1|1|cell-width|13fn>|<cwith|2|-1|1|1|cell-hmode|exact>|<table|<row|<\cell>
      <with|font-series|bold|Generic macros>
    </cell>|<\cell>
      \;
    </cell>>|<row|<\cell>
      <\compact>
        <\scm>
          (repeat n

          \ \ body <text-dots>)
        </scm>
      </compact>
    </cell>|<\cell>
      Evaluates <verbatim|body> <math|n> times.
    </cell>>|<row|<\cell>
      <scm|(inc! x)>
    </cell>|<\cell>
      Increments the value of <verbatim|x>. <verbatim|x> must be a variable.
    </cell>>|<row|<\cell>
      <scm|(dec! x)>
    </cell>|<\cell>
      Decrements the value of <verbatim|x>. <verbatim|x> must be a variable.
    </cell>>|<row|<\cell>
      <scm|(push! list x)>
    </cell>|<\cell>
      Adds the value <verbatim|x> at the front of <scm|list>. <scm|list> must
      be a variable.
    </cell>>|<row|<\cell>
      <scm|(pop! list)>
    </cell>|<\cell>
      Extracts and returns the element at the front of <scm|list>. <scm|list>
      must be a variable.
    </cell>>>>
  </wide-tabular>

  <\wide-tabular>
    <tformat|<cwith|1|3|1|2|cell-bsep|0.5fn>|<cwith|1|3|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|2|cell-bsep|0.5fn>|<cwith|1|1|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|2|cell-bsep|0.5fn>|<cwith|1|1|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|2|cell-bsep|0.5fn>|<cwith|1|1|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|1|cell-row-span|1>|<cwith|1|1|1|1|cell-col-span|2>|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|2|cell-tborder|1ln>|<cwith|1|1|1|2|cell-bborder|1ln>|<cwith|1|1|1|2|cell-lborder|0ln>|<cwith|1|1|1|2|cell-rborder|0ln>|<cwith|2|-1|1|1|cell-width|13fn>|<cwith|2|-1|1|1|cell-hmode|exact>|<table|<row|<\cell>
      <with|font-series|bold|Status messages>
    </cell>|<\cell>
      \;
    </cell>>|<row|<\cell>
      <scm|(status format <text-dots>)>
    </cell>|<\cell>
      Shows the formatted message on top of the player's head. <scm|format>
      can be a string or something with more structure: <scm|(status "I am
      Scheme-Man!")> or <scm|(status "sum: ~a, not: ~a" (+ 1 2 3) (not #t))>.
    </cell>>|<row|<\cell>
      <scm|(clear-status)>
    </cell>|<\cell>
      Equivalent to <scm|(status "")>.
    </cell>>>>
  </wide-tabular>

  <\wide-tabular>
    <tformat|<cwith|1|2|1|2|cell-bsep|0.5fn>|<cwith|1|2|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|2|cell-bsep|0.5fn>|<cwith|1|1|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|1|cell-row-span|1>|<cwith|1|1|1|1|cell-col-span|2>|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|2|cell-tborder|1ln>|<cwith|1|1|1|2|cell-bborder|1ln>|<cwith|1|1|1|2|cell-lborder|0ln>|<cwith|1|1|1|2|cell-rborder|0ln>|<cwith|2|2|1|1|cell-width|13fn>|<cwith|2|2|1|1|cell-hmode|exact>|<cwith|2|-1|1|-1|cell-bsep|0.5fn>|<cwith|2|-1|1|-1|cell-tsep|0.5fn>|<table|<row|<\cell>
      <with|font-series|bold|Checking and resetting the state of the game>
    </cell>|<\cell>
      \;
    </cell>>|<row|<\cell>
      <scm|(playing?)>
    </cell>|<\cell>
      Returns <scm|#f> only when the player has lost.
    </cell>>|<row|<\cell>
      <scm|(reset-level)>
    </cell>|<\cell>
      Resets the level the way it was first loaded but does not the state of
      the Scheme interpreter (all the definitions are kept).
    </cell>>>>
  </wide-tabular>

  <\wide-tabular>
    <tformat|<cwith|1|4|1|2|cell-bsep|0.5fn>|<cwith|1|4|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|1|cell-row-span|1>|<cwith|1|1|1|1|cell-col-span|2>|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|2|cell-tborder|1ln>|<cwith|1|1|1|2|cell-bborder|1ln>|<cwith|2|2|1|2|cell-tborder|1ln>|<cwith|1|1|1|2|cell-lborder|0ln>|<cwith|1|1|1|2|cell-rborder|0ln>|<cwith|2|3|1|1|cell-rsep|1fn>|<cwith|2|-1|1|1|cell-width|13fn>|<cwith|2|-1|1|1|cell-hmode|exact>|<cwith|2|-1|1|-1|cell-bsep|0.5fn>|<cwith|2|-1|1|-1|cell-tsep|0.5fn>|<table|<row|<\cell>
      <with|font-series|bold|Walking>
    </cell>|<\cell>
      \;
    </cell>>|<row|<\cell>
      <\compact>
        <\scm>
          (walk)
        </scm>

        <scm|(walk n)>
      </compact>
    </cell>|<\cell>
      Walks one or <math|n> steps in the direction the player is facing.
    </cell>>|<row|<\cell>
      <\scm>
        (walk-while predicate)
      </scm>
    </cell>|<\cell>
      Walks while <scm|predicate> evaluates to true.

      For example, <scm|(walk-while (see? 'coin))> walks while the player
      sees a coin in their line of sight.
    </cell>>|<row|<\cell>
      <\compact>
        <\scm>
          (go-back)
        </scm>

        <scm|(go-back n)>
      </compact>
    </cell>|<\cell>
      Walks one or <math|n> steps backwards from the direction the player is
      facing.
    </cell>>>>
  </wide-tabular>

  <\wide-tabular>
    <tformat|<cwith|1|2|1|2|cell-bsep|0.5fn>|<cwith|1|2|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|2|cell-bsep|0.5fn>|<cwith|1|1|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|1|cell-row-span|1>|<cwith|1|1|1|1|cell-col-span|2>|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|2|cell-tborder|1ln>|<cwith|1|1|1|2|cell-bborder|1ln>|<cwith|1|1|1|2|cell-lborder|0ln>|<cwith|1|1|1|2|cell-rborder|0ln>|<cwith|2|2|1|1|cell-rsep|1fn>|<cwith|2|2|1|1|cell-width|13fn>|<cwith|2|2|1|1|cell-hmode|exact>|<table|<row|<\cell>
      <with|font-series|bold|Turning>
    </cell>|<\cell>
      \;
    </cell>>|<row|<\cell>
      <scm|(turn dir)>
    </cell>|<\cell>
      Turns in the specified direction. The direction must be one of the
      following symbols: left, right or opposite. Example: <scm|(turn
      'right)>.
    </cell>>>>
  </wide-tabular>

  <\wide-tabular>
    <tformat|<cwith|1|2|1|2|cell-bsep|0.5fn>|<cwith|1|2|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|2|cell-bsep|0.5fn>|<cwith|1|1|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|2|cell-bsep|0.5fn>|<cwith|1|1|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|2|cell-bsep|0.5fn>|<cwith|1|1|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|1|cell-row-span|1>|<cwith|1|1|1|1|cell-col-span|2>|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|2|cell-tborder|1ln>|<cwith|1|1|1|2|cell-bborder|1ln>|<cwith|1|1|1|2|cell-lborder|0ln>|<cwith|1|1|1|2|cell-rborder|0ln>|<cwith|2|-1|1|1|cell-width|13fn>|<cwith|2|-1|1|1|cell-hmode|exact>|<cwith|2|-1|1|-1|cell-bsep|0.5fn>|<cwith|2|-1|1|-1|cell-tsep|0.5fn>|<table|<row|<\cell>
      <with|font-series|bold|Seeing>
    </cell>|<\cell>
      \;
    </cell>>|<row|<\cell>
      <\compact>
        <\scm>
          (see)

          (see distance)
        </scm>
      </compact>
    </cell>|<\cell>
      Returns a symbol with the name of the object that's right in front or
      at a specified <verbatim|distance> from the player. <verbatim|distance>
      must be a strictly positive number. Example: <verbatim|(see 2)> could
      return <verbatim|floor> or <verbatim|coin>.

      Returns an empty list if at that position there is no floor.

      If there is no floor between two patches of land, you will not be able
      to see and the return value will always be an empty list.
    </cell>>|<row|<\cell>
      <\compact>
        <scm|(see? object)>

        <scm|(see? object min-dist)>
      </compact>
    </cell>|<\cell>
      Returns <scm|#t> if an object of type <verbatim|object> is found at a
      distance at least one tile in the first case or <verbatim|min-dist>
      tiles in the second. Otherwise, this function returns <scm|#f>.
    </cell>>|<row|<\cell>
      <scm|(can-walk?)>
    </cell>|<\cell>
      Returns <scm|#t> if there is floor in front of the player for them to
      walk to.
    </cell>>>>
  </wide-tabular>

  \;

  <\wide-tabular>
    <tformat|<cwith|1|-1|1|-1|cell-bsep|0.5fn>|<cwith|1|-1|1|-1|cell-tsep|0.5fn>|<cwith|1|1|1|2|cell-bsep|0.5fn>|<cwith|1|1|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|2|cell-bsep|0.5fn>|<cwith|1|1|1|2|cell-tsep|0.5fn>|<cwith|1|1|1|1|cell-row-span|1>|<cwith|1|1|1|1|cell-col-span|2>|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|2|cell-tborder|1ln>|<cwith|1|1|1|2|cell-bborder|1ln>|<cwith|1|1|1|2|cell-lborder|0ln>|<cwith|1|1|1|2|cell-rborder|0ln>|<cwith|2|-1|1|1|cell-width|13fn>|<cwith|2|-1|1|1|cell-hmode|exact>|<table|<row|<\cell>
      <with|font-series|bold|Remembering and going back to places>
    </cell>|<\cell>
      \;
    </cell>>|<row|<\cell>
      <scm|(remember-place name)>
    </cell>|<\cell>
      Remembers the player's current location and gives it a <scm|name>.
    </cell>>|<row|<\cell>
      <scm|(go-back-to place-name)>
    </cell>|<\cell>
      Returns to a previously remembered place.
    </cell>>|<row|<\cell>
      <\compact>
        <\scm>
          (with-route-backwards

          \ \ body ...)
        </scm>
      </compact>
    </cell>|<\cell>
      Perform a series of actions in <scm|body> and then return to the place
      the player was before executing those actions.
    </cell>>>>
  </wide-tabular>

  \;
</body>

<\initial>
  <\collection>
    <associate|font|roman>
    <associate|font-family|rm>
    <associate|math-font|roman>
    <associate|page-medium|paper>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|?|1|../../../.TeXmacs/texts/scratch/no_name_53.tm>>
    <associate|auto-2|<tuple|?|3|../../../.TeXmacs/texts/scratch/no_name_53.tm>>
  </collection>
</references>