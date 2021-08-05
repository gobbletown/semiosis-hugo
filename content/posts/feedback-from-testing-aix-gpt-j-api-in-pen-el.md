+++
title = "Feedback from testing AIx GPT-J API in Pen.el"
author = ["Shane Mulligan"]
date = 2021-08-06T00:00:00+12:00
keywords = ["gpt", "aix"]
draft = false
+++

## Feedback {#feedback}


### Max tokens not working? {#max-tokens-not-working}

I adjust the `pen-aix` script to print the tokens at the end of the completion.

{{< highlight python "linenos=table, linenostart=1" >}}
#!/usr/bin/python3

from aixapi import AIxResource
import os

# Get your API Key at apps.aixsolutionsgroup.com

# model=os.environ.get("PEN_ENGINE"),

if __name__ == "__main__":
    api_key = os.environ.get("AIX_API_KEY")
    aix_resource = AIxResource(api_key)
    print(
        str(
            aix_resource.compose(
                os.environ.get("PEN_PROMPT"),
                response_length=int(os.environ.get("PEN_MAX_TOKENS")),
                top_p=float(os.environ.get("PEN_TOP_P")),
                temperature=float(os.environ.get("PEN_TEMPERATURE")),
                stop_sequence=os.environ.get("PEN_STOP_SEQUENCE"),
            )
            .get("data", dict())
            .get("text")
        )
    )
    print(os.environ.get("PEN_MAX_TOKENS"))
{{< /highlight >}}

It appears that max tokens (response\_length) is not being respected.

The same amount of text apperars for the 1 token completion, 5 token completion and 200 token completion.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/6ubtGnnIxuhD5JelAWcw8Gp5r" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/6ubtGnnIxuhD5JelAWcw8Gp5r.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/6ubtGnnIxuhD5JelAWcw8Gp5r.js" id="asciicast-6ubtGnnIxuhD5JelAWcw8Gp5r" async></script>


### Including the stop sequence in the completion {#including-the-stop-sequence-in-the-completion}

AIx includes the stop sequence at the end of
the completion where the OpenAI API does not.


### Multiple completions format {#multiple-completions-format}

If you do implement multiple copmletions, OpenAI's has this format:

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/Jgh4E42HbP8ZDBnULH4G50fTG" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/Jgh4E42HbP8ZDBnULH4G50fTG.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/Jgh4E42HbP8ZDBnULH4G50fTG.js" id="asciicast-Jgh4E42HbP8ZDBnULH4G50fTG" async></script>

OpenAI returns the entire prompt for each completion and appends the actual completion to each.

They delimit the completions as shown below:

However, this could be an indication that in
the future they don't want to lock in their
API for forwards completion only.

{{< highlight text "linenos=table, linenostart=1" >}}
===== Completion 1 =====
Shane went back to his place, turned out some lights, and stared with a face of gloom. He'd have to find somewhere to sleep. He returned to where the girl was sitting. 'The guy said he'd see if he could find some place for you to stay. He'll be back soon.'

She looked up at him and smiled. 'Thank you.'

Shane returned her smile, and stood, as the girl rose and joined him. They strolled back towards the street, leaving the lights of the town behind. The darkness was so deep the sky was obscured by the stars.

Shane steered her toward a small sitting room at the back of the house, which was empty. 'D'you wanna crash here?'

The girl frowned at him. 'You don't have to do that.'

Shane shrugged. 'You're too old to be sitting out there.'

'I'm not. I'm eighteen.'

Shane looked at her. 'Jesus,
===== Completion 2 =====
Shane went back to his place, turned out some lights, and stared with a face of gloom. He'd have to find somewhere to sleep. He returned to where the girl was sitting. 'The guy said he'd see if he could find some place for you to stay. He'll be back soon.'

She looked up at him and smiled. 'Thank you.'

Shane returned her smile, and stood, as the girl rose and joined him. They strolled back towards the street, leaving the lights of the town behind. The darkness was so deep the sky was obscured by the stars.

Shane steered her toward a small sitting room at the back of the house, which was empty. It was just as she'd seen it at the restaurant, and she would not have minded staying here tonight. The tables and chairs were almost all present and correct, and the room was warm and cosy. In the kitchen, however, she saw no food, and no sign of a toilet, but she decided not
===== Completion 3 =====
Shane went back to his place, turned out some lights, and stared with a face of gloom. He'd have to find somewhere to sleep. He returned to where the girl was sitting. 'The guy said he'd see if he could find some place for you to stay. He'll be back soon.'

She looked up at him and smiled. 'Thank you.'

Shane returned her smile, and stood, as the girl rose and joined him. They strolled back towards the street, leaving the lights of the town behind. The darkness was so deep the sky was obscured by the stars.

Shane steered her toward a small sitting room at the back of the house, which was empty. He turned on the light, and she sank down onto a small sofa. He sat down opposite, and the girl rested her head on his shoulder, and he stroked her hair. The tension between them had eased, and they were comfortable with each other. The last thing he wanted to do was to think about what
===== Completion 4 =====
Shane went back to his place, turned out some lights, and stared with a face of gloom. He'd have to find somewhere to sleep. He returned to where the girl was sitting. 'The guy said he'd see if he could find some place for you to stay. He'll be back soon.'

She looked up at him and smiled. 'Thank you.'

Shane returned her smile, and stood, as the girl rose and joined him. They strolled back towards the street, leaving the lights of the town behind. The darkness was so deep the sky was obscured by the stars.

Shane steered her toward a small sitting room at the back of the house, which was empty. The girl spoke of her life, and of the family she'd lived with. She seemed well informed. Shane listened to her, and, as the girl spoke, his mind turned thoughts to his own troubles.

Later, as she showered, he helped her with her clothes. The girl's face smiled as
{{< /highlight >}}