# Customizing ggplot for yourself or your organization — NICAR 2025

_Athena Chapekis, Pew Research Center_
<br> _Kaitlyn Radde, Pew Research Center_

This GitHub repository contains the files from a NICAR 2025 class on customizing ggplot functions for yourself or your organization's style guide.

To walk through the files as we did in that session, clone this repository. Then, use the `class.qmd` file and follow along in `nicar_theme.R` and `nicar_line_simple.R`. 

Below are some general tips and resources we recommend for getting started. For more detailed information, you can also see the slides we used [here](LINK TK). 

## Why wrappers?
Writing wrapper functions for ggplot eliminates the need for redundant and tedious styling code every time you make a new plot, and/or the need for extensive manual editing outside of R. While the initial setup can be time-consuming in the short term, it's much faster than building funcitons from scratch and can save time in the long run. 

## Function-writing principles
- **Don’t over-specify arguments.** Think about what users actually need to be able to control vs. what is in your style guide and should not change.
- **Keep conditional statements simple and separate.** The more complex your functions get, and if you want to add features later, the more difficult it will be if conditionals aren’t tidy.
- **Set up different functions similarly to one another.** If the structures and parameter names are more or less the same across your plotting functions, it will be easier for users to learn and you to remember how to use them. 


## Should you write a package?
- **Are people at your organization already using R?** If people are already using R for data analysis or visualization, you’ll have a built in user base. 
- **Is there a graphics style guide? Are several different people tasked with making charts?** A custom package can ensure different people are applying the same styles, without thinking about it and without repetitive code.
- **Can you commit to writing and maintaining the code base?** Will anybody do it with you?

If you answered yes to all/most of these questions, a package is probably a good solution for you. 
LINKS FROM PRESENTATION TK

## Other helpful resources and examples
- [Quosures](https://adv-r.hadley.nz/quasiquotation.html)
- [Unit testing](https://r-pkgs.org/testing-basics.html)
- [Documentation](https://bbc.github.io/rcookbook/), particularly for external release
