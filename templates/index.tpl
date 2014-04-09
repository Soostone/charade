<apply template="_base">
<ul>
  <itemListing fake="list 3 10">
    <li><title fake="enum titles.txt"/> <firstName fake="first-name"/> <lastName fake="last-name"/></li>
  </itemListing>
</ul>
<markdown>
# Some numbers
</markdown>
<p>Int <count fake="int"/>, decimal <probability fake="decimal"/></p>
<p>Int <count fake="int 21 65"/>, decimal <probability fake="decimal -1 1"/></p>
<markdown>
# A paragraph
</markdown>
<p><postBody fake="lorem 2"/></p>
<p>Active: <isActive fake="bool"/></p>
<p>Here's where the markdown tag is:</p>
<markdown file="test.md"/>
</apply>
