#version 130

in vec4 fragPos;
in vec3 texDir;

uniform samplerCube cubemap;

out vec4 fragColor;

void main() {
  fragColor = texture(cubemap, texDir);
}
