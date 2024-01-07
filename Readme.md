# Knowledge Retrieval and Task Automation: with GPT on Histories of Herodotus
Florian Klohn, 5 Dec 2023

``` r
#Preliminaries:
knitr::opts_chunk$set(message=FALSE, warning=FALSE, eval = TRUE) 

rm(list=ls())

library(openai)
library(htmltools)
library(stringr)

dir.create("Histories_GPT", showWarnings = FALSE)

# 0 Generate helper function ---------------------------------------------------

if (file.exists("Histories_GPT/gpt_out.RDS")) {
  
  gpt_out <- readRDS("Histories_GPT/gpt_out.RDS")
  
} else {
  
  gpt_out <- list()
  today <- as.character(Sys.Date())
  gpt_out[1] <- today
  
}
```

## Introduction

With the rise of generative AI, automatic text and image generation are
becoming [increasingly popular](https://huggingface.co/). The creative
capabilities of Large Language Models can be used in various domains
such as knowledge extraction or task automation (eg. Yang et al. 2023;
Kaddour et al. 2023; Naveed et al. 2023; Liu et al. 2023), and rapid
developments in this area underscore the immense potential of these
technologies. One trending topic in the current discussion is how to
efficiently query the engines (eg. Zamfirescu-Pereira et al. 2023; Chen
et al. 2023). We contribute to this discourse by using R-code to
interact with both *GPT-3.5* and *GPT-4* using the
[OpenAI-API](https://platform.openai.com/) on the *science of
historiography*.

We start by following up on a [previous
essay](https://boiled-data.github.io/Histories.html) on text mining the
book *Histories* by the Greek author *Herodotus* (484 – 425 BC) and
summarize the content of his book using the powerful capabilities of
OpenAI’s *GPT3.5-turbo*. First, we let *GPT* summarize the text, setting
the model’s *temperature* parameter to zero, which minimizes the random
component in the generated output.

``` r
# 1 Query on OpenAI-API: GPT3.5 ---------------------------------------------------
# helper function to query the openai-api:
get_completion <- function(prompt, model='gpt-3.5-turbo', temperature = 0) {
  messages <- list(list('role'='user', 'content'=prompt))            
  response <- create_chat_completion(model,
                                      messages = messages ,
                                      temperature = temperature,
                                      openai_api_key = Sys.getenv("OPENAI_API_KEY"), #add your openai key to the environment variables
                                      )
  return(response$choices$message.content)
  
  }

# 1 First example: Summarizes Herodotus

prompt_1 <- 'Summarize the content of the book \'Histories\' by the author Herodotus. Provide the corresponding references.'

if (length(gpt_out)<2) {
 
  sum_histories <- get_completion(prompt_1)
  
  gpt_out[2] <- sum_histories
  
}

HTML(paste("Prompt:<i>", prompt_1), "</i>", "<p></p>", "GPT3.5:<i>", str_replace_all(gpt_out[[2]], "\\\n", "<br>"), "</i>")
```

Prompt:<i> Summarize the content of the book 'Histories' by the author Herodotus. Provide the corresponding references. </i> <p></p> GPT3.5:<i> "Histories" is a historical work written by the ancient Greek historian Herodotus in the 5th century BCE. The book is a compilation of Herodotus' extensive research and travels, aiming to document the causes and events of the Greco-Persian Wars. It covers a wide range of topics, including the rise of the Persian Empire, the conflicts between the Greeks and Persians, and the cultural differences between the two civilizations.<br><br>Herodotus begins by providing an account of the origins and rise of the Persian Empire, detailing the reigns of Cyrus the Great, Cambyses, and Darius I. He then delves into the causes of the Greco-Persian Wars, discussing the Ionian Revolt and the subsequent Persian invasions of Greece. The book extensively covers the major battles of the wars, such as the Battle of Marathon, the Battle of Thermopylae, and the Battle of Salamis.<br><br>Throughout "Histories," Herodotus also explores various cultural aspects of the Greeks and Persians, comparing their customs, traditions, and religious beliefs. He includes anecdotes and stories about notable individuals, such as Croesus, Xerxes, and Leonidas, providing insights into their characters and motivations.<br><br>The book is divided into nine books or volumes, each focusing on different aspects of the wars and the civilizations involved. Herodotus employs a narrative style, combining historical accounts with personal observations and interviews. His work is considered one of the earliest examples of historical writing and has had a significant influence on subsequent historians.<br><br>References:<br>- Herodotus. "Histories." Translated by Robin Waterfield, Oxford University Press, 1998. </i>

<br> The resulting text is well-written and looks plausible at first
glance. The provided reference suggests that the summary is based on the
original book. However, we should acknowledge that GPT was trained on a
lot of data from many different resources and is not necessarily relying
just on the *Histories* itself when answering our questions.

## Adding GPT’s Temperature

In the next step, we would like to figure out specific topics of the
book. What messages was Herodotus trying to tell his readers beyond the
pure content of the book? When we ask GPT, let us consider some
experiments on GPT’s temperature parameter as well: To vary the
randomness in the output we subsequently try out the configurations 0,
0.5, 1, and 1.5. Values beyond 1.5 resulted in pseudo-language, blending
tokens from different languages.

``` r
# 2 Query on OpenAI GPT3.5: Introducing Temperature ---------------------------------------------------
prompt_2 <- "What knowledge and lessons can today's societies and people learn from the book \"Histories\" of \"Herodotus\"? Provide your answer in a list using hyphen bullets."

temp_vec <- seq(0, 1.5, 0.5) # set vector of temperature parameters

messages_histories <- list()

if (length(gpt_out)<3) {
  
  for (i in temp_vec) {
      answer <- get_completion(prompt_2, temperature=i) # query depending on temperature
      messages_histories <- c(messages_histories, list(c(i, answer))) # concat list + add used temperature
    
  }
    
  gpt_out[3] <- list(messages_histories) #concat list + add used temperature
  
}

length_list <- length(gpt_out) #length of output
answer_1 <- paste0('"', str_split(gpt_out[[3]][[1]][[2]], "-")[[1]][[2]], '"') # example

HTML(paste("Prompt:<i>", prompt_2), "</i>", "<br>")
```

Prompt:<i> What knowledge and lessons can today's societies and people learn from the book "Histories" of "Herodotus"? Provide your answer in a list using hyphen bullets. </i> <br>

`" The importance of recording and preserving historical events for future generations "`
is one element from the resulting list of `4` lists. We use these
answers for further exploration.

## Generative AI and Task Automation

To facilitate a comparative analysis of GPT’s responses, we reset the
model’s temperature to 0. Additionally, instead of using *GPT-3.5* we
now upgrade to *gpt-4-1106-preview*, which allows us to get more support
for highly complex language modeling tasks.

``` r
# 3 Query on OpenAI GPT4: Comparative Analysis ---------------------------------------------------
# create helper function to extract and combine arguments from our list:
gpt_get_element <- function(x) {

    paste0('Temperature ', gpt_out[[3]][[x]][[1]], ': ', gpt_out[[3]][[x]][[2]])
  
}

# extract list with answers from GPT:
gpt_arguments <- lapply(seq(1:length(gpt_out[[3]])), gpt_get_element) |>
                 paste(collapse=' ;; ') 


# 3 Compare output of step 2 ------------------------------------------------------------------------------------

# Combine query with input from above:
prompt_3 <- "You are given a list \"X\" of \" ;; \"-separated and titled lists \"y\", each containing statements on topics of the book \"Histories\" of \"Herodotus\". The statements are labeled with hyphen bullets. 
Do the following: 
- Compare the  contextual similarity of these statements between the lists \"y\" to identify the total number of meanings.
- Use your comparisons to sort the statements into one enumerated table \"Z\", such that the entries in the resulting rows have very similar meanings. The first column of table \"Z\" should contain ordered row numbers for each statement, from 1 to the total number of meanings in \"X\". Name the other columns by their corresponding name in \"y\". 
- Only output table \"Z\". Output this table in HTML format.

\"X\": " 


if (length(gpt_out)<4) {
 
   table_compare <- get_completion(paste(prompt_3, gpt_arguments), model = 'gpt-4-1106-preview') # do comparison
   gpt_out[4] <- table_compare # append output list

}

saveRDS(gpt_out, 'Histories_GPT/gpt_out.RDS') # save data for future compilation


HTML(paste("Prompt:<i>", prompt_3), "</i>", "<br>") # output prompt
```

Prompt:<i> You are given a list "X" of " ;; "-separated and titled lists "y", each containing statements on topics of the book "Histories" of "Herodotus". The statements are labeled with hyphen bullets. 
Do the following: 
- Compare the  contextual similarity of these statements between the lists "y" to identify the total number of meanings.
- Use your comparisons to sort the statements into one enumerated table "Z", such that the entries in the resulting rows have very similar meanings. The first column of table "Z" should contain ordered row numbers for each statement, from 1 to the total number of meanings in "X". Name the other columns by their corresponding name in "y". 
- Only output table "Z". Output this table in HTML format.
&#10;"X":  </i> <br>

``` r
HTML(str_extract(as.character(gpt_out[4]), "(?s)<table>(.*?)</table>")) 
```

<table>
  <tr>
    <th>Row Number</th>
    <th>Temperature 0</th>
    <th>Temperature 0.5</th>
    <th>Temperature 1</th>
    <th>Temperature 1.5</th>
  </tr>
  <tr>
    <td>1</td>
    <td>The importance of recording and preserving historical events for future generations</td>
    <td>The importance of recording and preserving historical events for future generations</td>
    <td>The importance of recording and documenting historical events and stories</td>
    <td>Importance of historical inquiry and analysis</td>
  </tr>
  <tr>
    <td>2</td>
    <td>The significance of cultural diversity and the need for understanding and respecting different cultures</td>
    <td>The significance of cultural diversity and tolerance</td>
    <td>The importance of understanding and appreciating diverse cultures and traditions</td>
    <td>Cultural diversity and the value of understanding different civilizations</td>
  </tr>
  <tr>
    <td>3</td>
    <td>The impact of geography and environment on the development of civilizations</td>
    <td>The exploration of the natural and geographical factors influencing human history</td>
    <td>The significance of geography and its impact on the development of societies</td>
    <td>Recognition of the impact of geography on a culture's development</td>
  </tr>
  <tr>
    <td>4</td>
    <td>The consequences of excessive pride and hubris in individuals and societies</td>
    <td></td>
    <td>The consequences of excessive pride and hubris</td>
    <td></td>
  </tr>
  <tr>
    <td>5</td>
    <td>The role of religion and belief systems in shaping societies and influencing historical events</td>
    <td>The analysis of the role of religion and mythology in shaping societies</td>
    <td>The role of religion and beliefs in shaping societies and their actions</td>
    <td></td>
  </tr>
  <tr>
    <td>6</td>
    <td>The power dynamics between different nations and the consequences of imperialism and colonization</td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>7</td>
    <td>The value of critical thinking and questioning established narratives and beliefs</td>
    <td>The understanding that different perspectives and biases shape historical narratives</td>
    <td>The value of multiple perspectives and questioning traditional and dominant narratives</td>
    <td>Critical examination of 'truths' and questioning of bias</td>
  </tr>
  <tr>
    <td>8</td>
    <td>The complexities of human nature and the range of motivations behind individual and collective actions</td>
    <td></td>
    <td></td>
    <td>Assessment of human behaviors, motivations, and achievements throughout history</td>
  </tr>
  <tr>
    <td>9</td>
    <td>The cyclical nature of history and the patterns that can be observed in the rise and fall of civilizations</td>
    <td>The examination of the rise and fall of civilizations</td>
    <td></td>
    <td>Reflection on the fragility of empires and personal eminence</td>
  </tr>
  <tr>
    <td>10</td>
    <td>The importance of diplomacy and peaceful resolutions in international relations</td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>11</td>
    <td></td>
    <td>The recognition of the interconnectedness of different cultures and societies</td>
    <td>The interconnectedness of different cultures and civilizations</td>
    <td>Appreciation for the interconnectedness of world events</td>
  </tr>
  <tr>
    <td>12</td>
    <td></td>
    <td>The exploration of the causes and consequences of wars and conflicts</td>
    <td>The impact of war and conflict on societies and individuals</td>
    <td>Understanding the effect of conflicts and wars on societies and individuals</td>
  </tr>
  <tr>
    <td>13</td>
    <td></td>
    <td>The appreciation for the power of storytelling and oral traditions</td>
    <td></td>
    <td>Reverence for storytelling and narrating events</td>
  </tr>
  <tr>
    <td>14</td>
    <td></td>
    <td>The study of the origins and development of political systems</td>
    <td></td>
    <td>Examination of political systems and leadership styles</td>
  </tr>
  <tr>
    <td>15</td>
    <td></td>
    <td>The examination of the role of economics and trade in shaping societies</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>16</td>
    <td></td>
    <td>The exploration of social structures and hierarchies within different civilizations</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>17</td>
    <td></td>
    <td>The recognition of the importance of historical context in interpreting events</td>
    <td></td>
    <td>Exploration of various perspectives and interpretations of history</td>
  </tr>
  <tr>
    <td>18</td>
    <td></td>
    <td>The examination of the role of women and gender dynamics in ancient societies</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>19</td>
    <td></td>
    <td>The understanding of the significance of art, architecture, and cultural achievements in shaping societies</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>20</td>
    <td></td>
    <td></td>
    <td>The significance of human agency in shaping historical events</td>
    <td></td>
  </tr>
  <tr>
    <td>21</td>
    <td></td>
    <td></td>
    <td></td>
    <td>Lessons of governance, both successes and failures</td>
  </tr>
  <tr>
    <td>22</td>
    <td></td>
    <td></td>
    <td></td>
    <td>Mitigation of ethnocentrism through a broader perspective of world history</td>
  </tr>
</table>

<br> A first exploration of the table shows that the mapping of the
arguments from the different temperature configurations works pretty
well. Although there is some variability between the lists, a lot of
arguments show up more than once in the table. Generated by GPT-4, the
summarized list contains the 22 distinct answers of GPT-3.5. These
answers range from ‘The significance of cultural diversity and
tolerance’ (4 times mentioned) to ‘The impact of war and conflict on
societies and individuals’ (3 times) to ‘The consequences of excessive
pride and hubris’ (2 times) and more.

## Summary

Overall, we see that the interplay of R-Programming and the OpenAI-API
can do a magnificent job of knowledge mining and generative text
analysis. We also see that prompt engineering strategies such as
providing *enough context* or formulating *specific expectations* are
useful tactics when interacting with AI, as well as varying the
temperature parameter, or switching the underlying model.

Are the suggested answers correct? Are important lessons missing from
the above overview? To answer these questions, we need to have a look at
the literature or the book ourselves. In general, such a semantic list
will probably not be conclusive, since interpretations of the text are
subjective, depending on the perspective of the reader.

In essence, we have seen that [old
texts](https://en.wikipedia.org/wiki/Lindy_effect) such as the
‘Histories’ can capture a lot of knowledge and wisdom that can be
discovered by either reading – or by accessing them through generative
AI. Our journey through the *Histories* has not only pointed towards a
richer understanding of historical narratives but also emphasized a
fruitful interplay between programming and machine intelligence.

Your perspectives enrich the discussion, turning it into a collaborative
exploration of the past and the future. What are your thoughts on this?

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-chen2023unleashing" class="csl-entry">

Chen, Banghao, Zhaofeng Zhang, Nicolas Langrené, and Shengxin Zhu. 2023.
“Unleashing the Potential of Prompt Engineering in Large Language
Models: A Comprehensive Review.” *arXiv Preprint arXiv:2310.14735*.

</div>

<div id="ref-html2023tools" class="csl-entry">

Cheng, Joe, Carson Sievert, Barret Schloerke, Winston Chang, Yihui Xie,
and Jeff Allen. 2023. *Htmltools: Tools for HTML*.
<https://CRAN.R-project.org/package=htmltools>.

</div>

<div id="ref-prompt2023eng" class="csl-entry">

Fulford, Isa, and Andrew Ng. 2023. “ChatGPT Prompt Engineering for
Developers.”
https://www.deeplearning.ai/short-courses/chatgpt-prompt-engineering-for-developers/.

</div>

<div id="ref-kaddour2023challenges" class="csl-entry">

Kaddour, Jean, Joshua Harris, Maximilian Mozes, Herbie Bradley, Roberta
Raileanu, and Robert McHardy. 2023. “Challenges and Applications of
Large Language Models.” *arXiv Preprint arXiv:2307.10169*.

</div>

<div id="ref-liu2023summary" class="csl-entry">

Liu, Yiheng, Tianle Han, Siyuan Ma, Jiayue Zhang, Yuanyuan Yang, Jiaming
Tian, Hao He, et al. 2023. “Summary of Chatgpt-Related Research and
Perspective Towards the Future of Large Language Models.”
*Meta-Radiology*, 100017.

</div>

<div id="ref-naveed2023comprehensive" class="csl-entry">

Naveed, Humza, Asad Ullah Khan, Shi Qiu, Muhammad Saqib, Saeed Anwar,
Muhammad Usman, Nick Barnes, and Ajmal Mian. 2023. “A Comprehensive
Overview of Large Language Models.” *arXiv Preprint arXiv:2307.06435*.

</div>

<div id="ref-openai2023wrap" class="csl-entry">

Rudnytskyi, Iegor. 2023. *Openai: R Wrapper for OpenAI API*.
<https://CRAN.R-project.org/package=openai>.

</div>

<div id="ref-stringr" class="csl-entry">

Wickham, Hadley. 2022. *Stringr: Simple, Consistent Wrappers for Common
String Operations*. <https://CRAN.R-project.org/package=stringr>.

</div>

<div id="ref-yang2023harnessing" class="csl-entry">

Yang, Jingfeng, Hongye Jin, Ruixiang Tang, Xiaotian Han, Qizhang Feng,
Haoming Jiang, Bing Yin, and Xia Hu. 2023. “Harnessing the Power of Llms
in Practice: A Survey on Chatgpt and Beyond.” *arXiv Preprint
arXiv:2304.13712*.

</div>

<div id="ref-zamfirescu2023johnny" class="csl-entry">

Zamfirescu-Pereira, JD, Richmond Y Wong, Bjoern Hartmann, and Qian Yang.
2023. “Why Johnny Can’t Prompt: How Non-AI Experts Try (and Fail) to
Design LLM Prompts.” In *Proceedings of the 2023 CHI Conference on Human
Factors in Computing Systems*, 1–21.

</div>

</div>
