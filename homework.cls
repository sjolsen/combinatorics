\ProvidesClass{homework}[2015/04/09 Homework class]
\LoadClass{article}

\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{amsfonts}
\RequirePackage{amsthm}
\RequirePackage{titlesec}

%% Section formatting
\renewcommand{\thesubsection}{\arabic{subsection}}
\renewcommand{\thesubsubsection}{\alph{subsubsection}}
\titleformat{\subsection}[runin]{\normalfont\large\bfseries}{\thesubsection.}{0em}{}[]
\titleformat{\subsubsection}[runin]{}{(\thesubsubsection)}{0em}{}[]
\titlespacing*{\subsection}{0pt}{4em}{1.5ex plus .2ex}
\titlespacing*{\subsubsection}{0pt}{2ex}{1.5ex plus .2ex}

%% Subsection-level "problem" command
\newsavebox\secindentbox
\newlength\secprewidth
\newlength\sectextwidth

\def\problem{\@ifstar\@problemstar\@problem}

\newcommand{\@problem}[2]{%
  \setcounter{subsection}{\the\numexpr#1-1}%
  \savebox{\secindentbox}{\normalfont\large\bfseries #1.\ \hspace{1.95831pt}}%
  \settowidth{\secprewidth}{\usebox{\secindentbox}}%
  \setlength{\sectextwidth}{\textwidth}%
  \addtolength{\sectextwidth}{-\secprewidth}%
  \subsection{}{\parbox[t]{\sectextwidth}{\large #2}}%
  \vspace{\baselineskip}%
}

\newcommand{\@problemstar}[1]{%
  {\large #1}%
}

\newcommand{\subproblem}[1]{%
  \subsubsection{}{#1}%
}



%%% Math tools
\newcommand{\card}[1]{\left\lvert#1\right\rvert}
\newcommand{\pset}{\mathcal{P}}
\newcommand{\suchthat}{:}
\newcommand{\divides}{\mathbin{|}}
\newcommand{\floor}[1]{\left\lfloor#1\right\rfloor}
\renewcommand{\emptyset}{\varnothing}
\DeclareMathOperator{\lcm}{lcm}
\newcommand{\answer}[1]{\boxed{#1}}
\newcommand{\compl}[1]{\overline{#1}}

\newcommand{\incomplete}[1]{%
  {\it Incomplete.}
}

%% Quantification
\newcommand{\quantify}[3]{%
  {#1}\,{#2}.\,{#3}%
}
\newcommand{\qsome}[2]{\quantify{\exists}{#1}{#2}}
\newcommand{\nqsome}[2]{\quantify{\nexists}{#1}{#2}}
\newcommand{\qall}[2]{\quantify{\forall}{#1}{#2}}
% FIXME: \nqall

%% Case analysis
% FIXME: Don't indent the first paragraph
\RequirePackage{environ}
\RequirePackage{enumitem}
\NewEnviron{caselist}{%
  \begin{itemize}[listparindent=\parindent]%
    \BODY%
  \end{itemize}%
}
\newcommand{\case}[2]{%
  \item {#1}:

    {#2}%
}
