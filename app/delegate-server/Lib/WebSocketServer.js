import WebSocket, { WebSocketServer } from "ws";

export const newWebSocketServer = (options) => () => {
  return new WebSocketServer(options);
};

export const onConnect = (wss) => (cb) => () => {
  wss.on("connection", (ws, req) => {
    ws.connPath = req.url;
    cb(ws)(ws.connPath)();
  });
};

export const sendMessage = (ws) => (message) => () => {
  ws.send(message);
};

export const closeConn = (ws) => (code) => (reason) => () => {
  ws.close(code, reason);
};

export const broadcastMessage = (wss) => (auctionCs) => (message) => () => {
  wss.clients.forEach((ws) => {
    if (ws.readyState === WebSocket.OPEN && ws.connPath === "/" + auctionCs) {
      ws.send(message, { binary: false });
    }
  });
};

export const closeWebSocketServer = (wss) => (done) => () => {
  wss.clients.forEach((ws) => ws.terminate());
  wss.close(done);
};
