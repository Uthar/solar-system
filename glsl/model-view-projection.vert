#version 130

in vec3 pos;

out vec4 fragPos;
out vec3 texDir;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
  fragPos = projection * view * model * vec4(pos, 1.0);
  texDir = pos;
  gl_Position = fragPos;
}
