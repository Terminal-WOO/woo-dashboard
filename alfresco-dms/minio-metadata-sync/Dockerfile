FROM node:22-alpine

WORKDIR /app

# Install dependencies
COPY package*.json ./
RUN npm ci --only=production

# Copy source code
COPY . .

# Build TypeScript
RUN npm install -g typescript && \
    npm run build && \
    npm uninstall -g typescript

# Remove dev dependencies and source
RUN npm prune --production && \
    rm -rf src tsconfig.json

# Run as non-root user
USER node

CMD ["node", "dist/index.js"]
