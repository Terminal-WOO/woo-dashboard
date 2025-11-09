FROM node:22-alpine

WORKDIR /app

# Install dependencies
COPY package*.json ./
RUN npm install

# Copy source code
COPY . .

# Build TypeScript
RUN npm run build

# Remove dev dependencies and source
RUN npm prune --production && \
    rm -rf src tsconfig.json

# Run as non-root user
USER node

EXPOSE 3000

CMD ["node", "dist/index.js"]
