\ProvidesClass{homework}[2015/04/09 Homework class]
\LoadClass{article}

\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{amsfonts}
\RequirePackage{titlesec}

%% Section formatting
\renewcommand{\thesubsection}{\arabic{subsection}}
\renewcommand{\thesubsubsection}{\alph{subsubsection}}
\titleformat{\subsection}[runin]{\normalfont\large\bfseries}{\thesubsection.}{0em}{}[]
\titleformat{\subsubsection}[runin]{\normalfont\large}{(\thesubsubsection)}{0em}{}[]
\titlespacing*{\subsection}{0pt}{4em}{1.5ex plus .2ex}
\titlespacing*{\subsubsection}{0pt}{2ex}{1.5ex plus .2ex}

%% Subsection-level "problem" command
\newsavebox\subsecindentbox
\newlength\subsecprewidth
\newlength\subsectextwidth

\def\problem{\@ifstar\@problemstar\@problem}

\newcommand{\@problem}[2]{%
  \setcounter{subsection}{\the\numexpr#1-1}%
  \savebox{\subsecindentbox}{\normalfont\large\bfseries #1.\ \hspace{1.95831pt}}%
  \settowidth{\subsecprewidth}{\usebox{\subsecindentbox}}%
  \setlength{\subsectextwidth}{\textwidth}%
  \addtolength{\subsectextwidth}{-\subsecprewidth}%
  \subsection{}{\parbox[t]{\subsectextwidth}{\large #2}}%
  \vspace{1.5ex plus .2ex}%
}

\newcommand{\@problemstar}[1]{%
  {\large #1}%
}

%% Subsubsection-level "subproblem command"
\newsavebox\subsubsecindentbox
\newlength\subsubsecprewidth
\newlength\subsubsectextwidth

\newcommand{\subproblem}[2]{%
  \setcounter{subsubsection}{\the\numexpr`#1-`a}%
  \savebox{\subsubsecindentbox}{(#1)\ }%
  \settowidth{\subsubsecprewidth}{\usebox{\subsubsecindentbox}}%
  \setlength{\subsubsectextwidth}{\textwidth}%
  \addtolength{\subsubsectextwidth}{-\subsubsecprewidth}%
  \subsubsection{}{\parbox[t]{\subsubsectextwidth}{\large #2}}%
  \vspace{1.5ex plus .2ex}%
}



%%% Math tools
\newcommand{\card}[1]{\left\lvert#1\right\rvert}
\newcommand{\pset}{\mathcal{P}}
\newcommand{\suchthat}{:}
\newcommand{\divides}{\mathbin{|}}
\newcommand{\ndivides}{\mathbin{\nmid}} % FIXME: ugly
\newcommand{\floor}[1]{\left\lfloor#1\right\rfloor}
\renewcommand{\emptyset}{\varnothing}
\DeclareMathOperator{\lcm}{lcm}
\newcommand{\answer}[1]{\boxed{#1}}
\newcommand{\compl}[1]{\overline{#1}}
\newcommand{\bn}{\mathbb{N}}
\newcommand{\bz}{\mathbb{Z}}

\newcommand{\incomplete}[1]{%
  {\it Incomplete.}
}

%% Theorem stuff
\RequirePackage{amsthm}
\newtheorem{lemma}{Lemma}
\newtheorem*{definition}{Definition}

%% Quantification
\newcommand{\quantify}[3]{%
  {#1}\,{#2}\,.\,{#3}%
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
