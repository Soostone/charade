<apply template="base">
<ul>
  <itemListing fake="loop 3 10">
    <li><firstName fake="first-name"/> <lastName fake="last-name"/></li>
  </itemListing>
</ul>
<p>Int <count fake="int"/>, decimal <probability fake="decimal"/></p>
<p>Int <count fake="int 21 65"/>, decimal <probability fake="decimal -1 1"/></p>
<p><postBody fake="lorem 2"/></p>
<p>Active: <isActive fake="bool"/></p>
</apply>
